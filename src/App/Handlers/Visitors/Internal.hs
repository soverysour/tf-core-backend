{-# LANGUAGE OverloadedStrings #-}

module App.Handlers.Visitors.Internal
  ( constructTransactionsDetails
  , consTransDetails
  , consUserWithReview
  ) where

import           Data.List                                (partition)
import qualified Data.Map.Strict                          as M
import           Data.Maybe                               (fromMaybe)
import qualified Data.Text                                as T
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions (unSerial)
import           Database.Beam.Postgres
import           Servant

import           App.Defs.Db
import           App.Defs.Logging
import           App.Models.Item.Queries
import           App.Models.Transaction.Queries
import           App.Models.UserAccount.Queries
import           App.Models.UserReview.Queries
import           App.Resources
import           App.Transfer.Objects
import           Inventory.Transfer.Objects

constructTransactionsDetails ::
     Resources -> [Transaction] -> Handler [TTransactionWithDetails]
constructTransactionsDetails _ [] = return []
constructTransactionsDetails res transactions = do
  let authorKeys = _transactionAuthorId <$> transactions
      transactionKeys = pk <$> transactions
  logIt res ("Started construction of transaction details." :: T.Text)
  authorsAndAvgs <-
    runQuery res $ runSelectReturningList $ selectUsersAndRanks authorKeys
  items <-
    runQuery res $
    runSelectReturningList $ selectItemsForTransactions transactionKeys
  let (wtsItems, wtbItems) = partition _itemSellingOrBuying items
      wtsItems' =
        M.fromListWith (++) $ fmap (\x -> (_itemTransactionId x, [x])) wtsItems
      wtbItems' =
        M.fromListWith (++) $ fmap (\x -> (_itemTransactionId x, [x])) wtbItems
      authors =
        M.fromList $
        fmap
          (\(userId, avg, user) -> (userId, (user, round avg)))
          authorsAndAvgs
  logIt res ("Traversing for transaction details." :: T.Text)
  traverse (consTransDetails res authors wtsItems' wtbItems') transactions

consTransDetails ::
     Resources
  -> M.Map UserAccountId (UserAccount, Int)
  -> M.Map TransactionId [Item]
  -> M.Map TransactionId [Item]
  -> Transaction
  -> Handler TTransactionWithDetails
consTransDetails res authors wtsItems wtbItems transaction =
  let userId = _transactionAuthorId transaction
   in case userId `M.lookup` authors of
        Nothing -> do
          logIt res $
            "Probable DB inconsistency, author (" <>
            T.pack (show userId) <> ") not found."
          throwError err500
        Just (user, avg) ->
          return $
          TTransactionWithDetails transactionInfo user' wtsItems' wtbItems'
          where wtsItems' =
                  itemToTItem <$> fromMaybe [] (transKey `M.lookup` wtsItems)
                wtbItems' =
                  itemToTItem <$> fromMaybe [] (transKey `M.lookup` wtbItems)
                transactionInfo =
                  TTransaction
                    (unSerial $ unTransactionKey transKey)
                    (_transactionCreatedAt transaction)
                user' =
                  TUserDetails
                    (unSerial $ unAccountKey userId)
                    (_userNickname user)
                    avg
                transKey = pk transaction

itemToTItem :: Item -> TItem
itemToTItem item = TItem name desc url props
  where
    name = _itemName item
    desc = _itemDescription item
    url = _itemIconUrl item
    props =
      case _itemProperties item of
        PgJSONB props' -> props'

consUserWithReview ::
     M.Map UserAccountId UserReview
  -> M.Map UserAccountId Double
  -> (UserAccountId, Int, UserAccount)
  -> Maybe TReview
consUserWithReview userToReview userToRank (userId, avg, user) =
  case userId `M.lookup` userToReview of
    Nothing -> Nothing
    Just review -> Just tReview
      where tReview = TReview (_userReviewValue review) tUserDetails tUserRank
            tUserDetails =
              TUserDetails
                (unSerial . unAccountKey $ pk user)
                (_userNickname user)
                avg
            tUserRank = round <$> (userId `M.lookup` userToRank)
