{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App
  ( run
  ) where

import           Control.Concurrent.Classy.Chan
import           Control.Exception                      (Exception, throw)
import           Control.Monad                          (unless, when)
import           Data.Aeson                             (decode', encode)
import qualified Data.ByteString.Lazy                   as BS
import           Data.Pool
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import           Data.Time.Clock.Compat                 (secondsToNominalDiffTime)
import           Database.Beam.Postgres
import           Network.Wai                            (Middleware)
import qualified Network.Wai.Handler.Warp               as Warp
import           Network.Wai.Middleware.Cors            (cors, corsMethods,
                                                         corsRequestHeaders,
                                                         simpleCorsResourcePolicy)
import           Network.Wai.Middleware.Servant.Options (provideOptions)
import           Servant
import           Servant.Auth.Server
import           Servant.Foreign                        ()
import           System.Log.FastLogger

import           App.Api
import           App.Config
import           App.Defs.Handler
import           App.Defs.Logging
import           App.Handlers
import           App.Migration
import           App.Models.UserAccount
import           App.Resources
import           Inventory.Providers                    (hasProviders)
import           Orphans                                ()

staticServer :: Server StaticApi
staticServer = serveDirectoryWebApp "static-files"

authServer :: WithResources (Server AuthApi)
authServer res = registerAccount res :<|> loginAccount res

visitorServer :: WithResources (Server VisitorApi)
visitorServer res =
  listTransactions res :<|> getTransactionDetails res :<|>
  filterTransactions res :<|>
  seeUserDetails res :<|>
  seeUserReviews res

traderServer :: WithResources (AuthResult UserJwtData -> Server TraderApi)
traderServer res (Authenticated user) =
  transactionCreate res user :<|> transactionClose res user :<|>
  userRank res user :<|>
  userReview res user
traderServer _ _ = throwAll err401

inventoryServer :: WithResources (AuthResult UserJwtData -> Server InventoryApi)
inventoryServer res (Authenticated user) =
  linkedProviders res user :<|> inventoryForProvider res user :<|>
  itemsForInventory res user
inventoryServer _ _ = throwAll err401

steamServer :: WithResources (AuthResult UserJwtData -> Server SteamApi)
steamServer res (Authenticated user) = validateSteamOpenIdRequest res user
steamServer _ _                      = throwAll err401

server :: WithResources (Server Api)
server res =
  staticServer :<|> visitorServer res :<|> authServer res :<|> traderServer res :<|>
  inventoryServer res :<|>
  steamServer res

app :: WithResources Application
app res = withContext $ server res
  where
    withContext =
      serveWithContext
        (Proxy @Api)
        (defaultCookieSettings :. _jwtCfg res :. EmptyContext)

data MkAppException =
  NoProvidersException
  deriving (Eq, Show)

instance Exception MkAppException

mkApp :: AppConf -> IO Application
mkApp conf@(AppConf runMigrations connStr maxPageSize loggerBufferSize providers port jwtKey') = do
  logger <- newStdoutLoggerSet loggerBufferSize
  unless (hasProviders providers) $ do
    pushLogStrLn
      logger
      (toLogStr
         ("App configuration implies no inventory providers, which is not allowed." :: T.Text))
    throw NoProvidersException
  pool <-
    createPool
      (connectPostgreSQL $ T.encodeUtf8 connStr)
      close
      1
      (secondsToNominalDiffTime 300)
      50
  jwtKey'' <-
    case jwtKey' >>= (decode' . BS.fromStrict) of
      Nothing -> do
        k <- generateKey
        pushLogStrLn
          logger
          (toLogStr $
           "Generated a new JWT Key: " <>
           (T.decodeUtf8 . BS.toStrict . encode) k <> ".")
        return k
      Just key -> return key
  transactionChan <- newChan
  let jwtCfg = defaultJWTSettings jwtKey''
      res = Resources pool jwtCfg maxPageSize transactionChan logger providers
  when runMigrations $ do
    migrateAll res
    logIt res ("Migrations done." :: T.Text)
  logIt res $ "Application started on port " <> T.pack (show port) <> "."
  logIt res $ "Running with configuration: " <> T.pack (show conf)
  return $ app res

run :: AppConf -> IO ()
run conf =
  mkApp conf >>= Warp.run (appPort conf) . cors' . provideOptions (Proxy @Api)

cors' :: Middleware
cors' = cors (const $ Just simpleCorsResourcePolicy')
  where
    simpleCorsResourcePolicy' =
      simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST", "DELETE"]
        , corsRequestHeaders = ["Content-Type", "Authorization"]
        }
