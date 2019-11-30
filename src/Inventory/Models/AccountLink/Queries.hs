module Inventory.Models.AccountLink.Queries
  ( selectLinkedInventoriesByUserId
  , selectLinkedInventoryForUserAndProvider
  , module Inventory.Models.AccountLink
  ) where

import qualified Data.Text                    as T
import           Database.Beam
import           Database.Beam.Postgres

import           App.Models
import           App.Models.UserAccount
import           Inventory.Models.AccountLink

selectLinkedInventoriesByUserId :: UserAccountId -> SqlSelect Postgres Int
selectLinkedInventoriesByUserId userId =
  select $
  fmap _linkedInventoryProvider $
  filter_
    (\link ->
       (unAccountKey . _linkedUserId) link ==. (val_ . unAccountKey) userId) $
  all_ (_accountLinks tradeForallDb)

selectLinkedInventoryForUserAndProvider ::
     UserAccountId -> Int -> SqlSelect Postgres T.Text
selectLinkedInventoryForUserAndProvider userId enumI =
  select $
  fmap _linkedAccountPayload $
  filter_
    (\link ->
       (unAccountKey . _linkedUserId) link ==. (val_ . unAccountKey) userId &&.
       _linkedInventoryProvider link ==.
       val_ enumI) $
  all_ (_accountLinks tradeForallDb)
