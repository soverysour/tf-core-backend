{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module App.Models.UserAccount
  ( UserAccountT(..)
  , UserAccount
  , UserAccountId
  , UserJwtData
  , jwtFromUser
  , unAccountKey
  ) where

import           Control.Lens.Operators
import           Crypto.JOSE.Error                        as Jose
import           Data.Aeson
import qualified Data.ByteString                          as B
import qualified Data.ByteString.Lazy                     as BLazy
import           Data.Int                                 (Int64)
import           Data.Swagger
import           Data.Text
import           Data.Time.Clock
import           Data.Time.Clock.Compat                   (secondsToNominalDiffTime)
import           Data.Time.LocalTime
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions (SqlSerial (..))
import           Servant.API
import           Servant.Auth.Server

import           Orphans                                  ()

data UserAccountT f =
  UserAccount
    { _userId        :: Columnar f (SqlSerial Int64)
    , _userNickname  :: Columnar f Text -- UNIQUE
    , _userEmail     :: Columnar f Text -- UNIQUE
    , _userPassword  :: Columnar f Text
    , _userBanReason :: Columnar f (Maybe Text)
    , _userCreatedAt :: Columnar f LocalTime
    , _userBannedAt  :: Columnar f (Maybe LocalTime)
    }
  deriving (Generic, Beamable)

type UserAccount = UserAccountT Identity

deriving instance Show UserAccount

deriving instance Eq UserAccount

type UserAccountId = PrimaryKey UserAccountT Identity

deriving instance Show UserAccountId

deriving instance Eq UserAccountId

deriving instance Ord UserAccountId

deriving instance ToJSON UserAccountId

instance Table UserAccountT where
  data PrimaryKey UserAccountT f = UserAccountId{unAccountKey ::
                                               Columnar f (SqlSerial Int64)}
                                   deriving (Generic, Beamable)
  primaryKey = UserAccountId . _userId

type Nickname = Text

type Email = Text

type UserJwtData = (Nickname, Email)

jwtFromUser :: JWTSettings -> UserAccount -> IO (Either Jose.Error B.ByteString)
jwtFromUser jwtCfg (UserAccount _ nick _email _ _ _ _) = do
  expirationTime <-
    addUTCTime (secondsToNominalDiffTime $ 60 * 60 * 8) <$> getCurrentTime
  possibleToken <- makeJWT (nick, _email) jwtCfg (Just expirationTime)
  return $ BLazy.toStrict <$> possibleToken

instance ToJSON UserAccount

instance FromJSON UserAccount

instance ToSchema UserAccountId where
  declareNamedSchema _ =
    return $ NamedSchema (Just "UserID") $ mempty & type_ ?~ SwaggerInteger

instance ToParamSchema UserAccountId where
  toParamSchema _ = mempty & type_ ?~ SwaggerInteger

instance FromHttpApiData UserAccountId where
  parseQueryParam t =
    UserAccountId . SqlSerial <$>
    (parseQueryParam :: Text -> Either Text Int64) t
