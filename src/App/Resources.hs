module App.Resources
  ( Resources(..)
  ) where

import           Control.Concurrent.Classy.Chan
import           Data.Pool
import           Database.Beam.Postgres
import           Servant.Auth.Server
import           System.Log.FastLogger

import           App.Transfer.Objects
import           Inventory.Providers

data Resources =
  Resources
    { _connPool        :: Pool Connection
    , _jwtCfg          :: JWTSettings
    , _maxPageSize     :: Integer
    , _transactionChan :: Chan IO TTransactionWithDetails
    , _logger          :: LoggerSet
    , _providers       :: Providers
    }
