module App.Config
  ( AppConf(..)
  ) where

import           Data.ByteString
import           Data.Text

import           Inventory.Providers

data AppConf =
  AppConf
    { mustRunMigrations  :: Bool
    , psqlConnStr        :: Text
    , maxPagingPageSize  :: Integer
    , logBufSize         :: Int
    , inventoryProviders :: Providers
    , appPort            :: Int
    , jwtKey             :: Maybe ByteString
    }
  deriving (Show, Eq)
