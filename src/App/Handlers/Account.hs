{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Handlers.Account
  ( registerAccount
  , loginAccount
  ) where

import           Control.Monad                       (when)
import           Control.Monad.Catch                 (catch)
import           Control.Monad.IO.Class              (liftIO)
import           Crypto.KDF.BCrypt
import           Crypto.Random
import qualified Data.ByteString                     as B
import           Data.Maybe                          (isJust)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import           Database.Beam
import           Database.PostgreSQL.Simple.Internal (SqlError)
import           Servant

import           App.Defs.Db
import           App.Defs.Handler
import           App.Defs.Logging
import           App.Models
import           App.Models.UserAccount.Queries
import           App.Resources
import           App.Transfer.Objects

registerAccount :: WithResources (TUserRegister -> Handler NoContent)
registerAccount res (TUserRegister nick email password) = do
  drg <- liftIO getSystemDRG
  logIt res $ "Registering account: " <> nick <> ", " <> email <> "."
  let (salt :: B.ByteString) = fst $ randomBytesGenerate 16 drg
      saltedPassword = bcrypt 10 salt (T.encodeUtf8 password)
      performInsert =
        runQuery res $
        runInsert $
        insert (_userAccounts tradeForallDb) $
        insertExpressions
          [ UserAccount
              default_
              (val_ nick)
              (val_ email)
              (val_ . T.decodeLatin1 $ saltedPassword)
              (val_ Nothing)
              currentTimestamp_
              (val_ Nothing)
          ]
      recover =
        (const $ do
           logIt res ("Recovering from failed registration." :: T.Text)
           throwError err400) :: SqlError -> Handler NoContent
  flip catch recover $ do
    performInsert
    logIt res ("Account successfully registered." :: T.Text)
    return NoContent

loginAccount :: WithResources (TUserLogin -> Handler TAuthToken)
loginAccount res (TUserLogin email password) = do
  let jwtCfg = _jwtCfg res
  logIt res $ "Logging into account: " <> email <> "."
  possibleResult <-
    runQuery res $ runSelectReturningOne $ selectUserByEmail email
  result <-
    case possibleResult of
      Nothing -> do
        logIt res ("Account not found." :: T.Text)
        throwError err400
      Just user@(UserAccount _ _ _ hash _ _ _) -> do
        when (isJust $ _userBannedAt user) $ do
          logIt res $ "Account banned, cannot log in: " <> email <> "."
          throwError err403
        if validatePassword (T.encodeUtf8 password) (T.encodeUtf8 hash)
          then liftIO $ jwtFromUser jwtCfg user
          else do
            logIt res ("Bad password." :: T.Text)
            throwError err400
  case result of
    Left _ -> do
      logIt res ("Error creating jwtFromUser..." :: T.Text)
      throwError err500
    Right token -> return . TAuthToken $ T.decodeLatin1 token
