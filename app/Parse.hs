{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( mkConf
  , AllConf(..)
  ) where

import qualified Data.ByteString            as B
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Vector                as V
import           Text.Toml

import           App.Config                 (AppConf (AppConf))
import           Inventory.Providers
import           Inventory.Transfer.Objects

data AllConf =
  AllConf
    { _ekgPort :: Int
    , _appConf :: AppConf
    }
  deriving (Eq, Show)

mkConf :: Table -> Maybe AllConf
mkConf table = AllConf <$> ekgPort <*> appConf
  where
    mustRunMigrations = "runMigrations" `HM.lookup` table >>= fromNodeBool
    sqlConnStr = "psqlConnStr" `HM.lookup` table >>= fromNodeStr
    maxPageSize = "maxPageSize" `HM.lookup` table >>= fromNodeInteger
    logBufSize = "logBufSize" `HM.lookup` table >>= fromNodeInt
    ekgPort = "ekgPort" `HM.lookup` table >>= fromNodeInt
    appPort = "appPort" `HM.lookup` table >>= fromNodeInt
    steamProvider =
      "steamInventoryProvider" `HM.lookup` table >>= fromNodeSteamProvider
    jwtKey = Just $ "jwtKey" `HM.lookup` table >>= fromNodeBs
    providers = Just $ Providers steamProvider
    appConf =
      AppConf <$> mustRunMigrations <*> sqlConnStr <*> maxPageSize <*>
      logBufSize <*>
      providers <*>
      appPort <*>
      jwtKey

fromNodeSteamProvider :: Node -> Maybe SteamProvider
fromNodeSteamProvider (VTable table) = do
  inventories <-
    "additionalInventories" `HM.lookup` table >>= fromNodeList >>=
    traverse fromNodeInventory
  onlyExtraInventories <-
    "useOnlyAdditionalInventories" `HM.lookup` table >>= fromNodeBool
  return $ SteamProvider inventories onlyExtraInventories
fromNodeSteamProvider _ = Nothing

fromNodeInventory :: Node -> Maybe TInventory
fromNodeInventory (VArray arr) =
  case V.toList arr of
    [nodeName, nodeId, nodeLogo] -> do
      name <- fromNodeStr nodeName
      id' <- fromNodeStr nodeId
      logo <- fromNodeStr nodeLogo
      return $ TInventory name id' logo
    _ -> Nothing
fromNodeInventory _ = Nothing

fromNodeBool :: Node -> Maybe Bool
fromNodeBool (VBoolean s) = Just s
fromNodeBool _            = Nothing

fromNodeStr :: Node -> Maybe T.Text
fromNodeStr (VString s) = Just s
fromNodeStr _           = Nothing

fromNodeBs :: Node -> Maybe B.ByteString
fromNodeBs (VString s) = Just . T.encodeUtf8 $ s
fromNodeBs _           = Nothing

fromNodeInt :: Node -> Maybe Int
fromNodeInt (VInteger i) = Just . fromInteger . toInteger $ i
fromNodeInt _            = Nothing

fromNodeInteger :: Node -> Maybe Integer
fromNodeInteger (VInteger i) = Just . toInteger $ i
fromNodeInteger _            = Nothing

fromNodeList :: Node -> Maybe [Node]
fromNodeList (VArray arr) = Just . V.toList $ arr
fromNodeList _            = Nothing
