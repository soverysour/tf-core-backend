module App.Models.Item.Queries
  ( selectItemsForTransactions
  , insertTransactionItemsToHist
  , module App.Models.Item
  ) where

import           Database.Beam
import           Database.Beam.Postgres

import           App.Models
import           App.Models.Item
import           App.Models.Transaction

selectItemsForTransactions :: [TransactionId] -> SqlSelect Postgres Item
selectItemsForTransactions transactionIds =
  select $
  filter_
    (\item ->
       unTransactionKey (_itemTransactionId item) `in_`
       (val_ . unTransactionKey <$> transactionIds)) $
  all_ (_items tradeForallDb)

insertTransactionItemsToHist :: TransactionId -> SqlInsert Postgres ItemT
insertTransactionItemsToHist transactionId =
  insert (_items_history tradeForallDb) $
  insertFrom $
  filter_
    (\item ->
       unTransactionKey (_itemTransactionId item) ==.
       (val_ . unTransactionKey) transactionId) $
  all_ (_items tradeForallDb)
