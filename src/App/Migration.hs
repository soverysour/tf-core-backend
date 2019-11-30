module App.Migration
  ( migrateAll
  ) where

import           Database.Beam.Migrate.Simple
import           Database.Beam.Postgres.Migrate

import           App.Defs.Db
import           App.Models
import           App.Resources

migrateAll :: Resources -> IO ()
migrateAll res =
  runQuery res $ autoMigrate migrationBackend tradeForallCheckedDb
