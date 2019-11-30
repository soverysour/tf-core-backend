{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Models.Transaction.Queries
  ( selectTransactions
  , selectTransactionsCount
  , selectFilterTransactions
  , selectFilterTransactionsCount
  , selectTransactionDetails
  , deleteTransaction
  , module App.Models.Transaction
  ) where

import qualified Data.Text                  as T
import           Database.Beam
import           Database.Beam.Postgres

import           App.Models
import           App.Models.Item
import           App.Models.Transaction
import           App.Pagination
import           Inventory.Transfer.Objects (TItemProps)

selectFilterTransactions ::
     SortPageCriteria TransactionOrdering
  -> TItemProps
  -> T.Text
  -> T.Text
  -> SqlSelect Postgres Transaction
selectFilterTransactions (SortPageCriteria criteria order offset limit) filterOpts nameFilter descFilter =
  case criteria of
    TransByCreated -> selection (sortOrderToOrder order . _transactionCreatedAt)
  where
    selection o =
      select $
      limit_ limit $
      offset_ offset $
      orderBy_ o $
      nub_ $ do
        item <- all_ (_items tradeForallDb)
        guard_
          (_itemProperties item @> (val_ . PgJSONB) filterOpts &&.
           _itemName item `like_`
           val_ ("%" <> nameFilter <> "%") &&.
           _itemDescription item `like_`
           val_ ("%" <> descFilter <> "%"))
        related_ (_transactions tradeForallDb) (_itemTransactionId item)

selectFilterTransactionsCount ::
     TItemProps -> T.Text -> T.Text -> SqlSelect Postgres Int
selectFilterTransactionsCount filterOpts nameFilter descFilter =
  select $
  aggregate_ (const countAll_) $
  nub_ $ do
    item <- all_ (_items tradeForallDb)
    guard_
      (_itemProperties item @> (val_ . PgJSONB) filterOpts &&. _itemName item `like_`
       val_ ("%" <> nameFilter <> "%") &&.
       _itemDescription item `like_`
       val_ ("%" <> descFilter <> "%"))
    related_ (_transactions tradeForallDb) (_itemTransactionId item)

selectTransactions ::
     SortPageCriteria TransactionOrdering -> SqlSelect Postgres Transaction
selectTransactions (SortPageCriteria criteria order offset limit) =
  case criteria of
    TransByCreated -> selection (sortOrderToOrder order . _transactionCreatedAt)
  where
    selection o =
      select $
      limit_ limit $
      offset_ offset $ orderBy_ o $ all_ (_transactions tradeForallDb)

selectTransactionsCount :: SqlSelect Postgres Int
selectTransactionsCount =
  select $ aggregate_ (const countAll_) $ all_ (_transactions tradeForallDb)

selectTransactionDetails :: [TransactionId] -> SqlSelect Postgres Transaction
selectTransactionDetails transactionIds =
  select $
  filter_
    (\transaction ->
       unTransactionKey (pk transaction) `in_`
       (val_ . unTransactionKey <$> transactionIds)) $
  all_ (_transactions tradeForallDb)

deleteTransaction :: TransactionId -> SqlDelete Postgres TransactionT
deleteTransaction transactionId =
  delete
    (_transactions tradeForallDb)
    (\transaction ->
       unTransactionKey (pk transaction) ==.
       (val_ . unTransactionKey) transactionId)
