module App.Defs.Logging
  ( logIt
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           System.Log.FastLogger

import           App.Resources

logIt :: (MonadIO m, ToLogStr msg) => Resources -> msg -> m ()
logIt res = liftIO . pushLogStrLn logger . toLogStr
  where
    logger = _logger res
