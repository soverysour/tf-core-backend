module App.Defs.Db
  ( runQuery
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Pool
import           Database.Beam.Postgres

import           App.Defs.Logging
import           App.Resources

runQuery :: MonadIO m => Resources -> Pg a -> m a
runQuery res query =
  liftIO $
  withResource pool $ \conn -> runBeamPostgresDebug (logIt res) conn query
  where
    pool = _connPool res
