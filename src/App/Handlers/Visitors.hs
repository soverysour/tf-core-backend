{-# LANGUAGE OverloadedStrings #-}

module App.Handlers.Visitors
  ( listTransactions
  , getTransactionDetails
  , filterTransactions
  , seeUserDetails
  , seeUserReviews
  ) where

import qualified Data.Aeson.Text                          as TA
import qualified Data.Map.Strict                          as M
import qualified Data.Text                                as T
import qualified Data.Text.Lazy                           as TL
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions (unSerial)
import           Servant

import           App.Defs.Db
import           App.Defs.Handler
import           App.Defs.Logging
import           App.Handlers.Visitors.Internal
import           App.Models.Transaction.Queries
import           App.Models.UserAccount.Queries
import           App.Models.UserRank.Queries
import           App.Models.UserReview.Queries
import           App.Pagination
import           App.Resources
import           App.Transfer.Objects

seeUserDetails :: ByIdHandler UserAccountId TUserDetails
seeUserDetails res userId = do
  logIt res $ "Trying to see user details for: " <> T.pack (show userId) <> "."
  logIt res $ T.pack $ show userId
  maybeRes <-
    runQuery res $ runSelectReturningOne $ selectUsersAndRanks [userId]
  case maybeRes of
    Just (userId', avg, user) -> do
      logIt res $
        "Obtained user details for: " <>
        T.pack (show user) <> ", with avg: " <> T.pack (show avg) <> "."
      return $
        TUserDetails
          (unSerial $ unAccountKey userId')
          (_userNickname user)
          (round avg)
    Nothing -> do
      logIt res ("Failed to see the user details." :: T.Text)
      throwError err404

seeUserReviews ::
     WithSortPageAlso UserAccountId ReviewOrdering (Handler (TPaginated TReview))
seeUserReviews res userId =
  withSPcrit (_maxPageSize res) ReviewByCreated $ \spCrit -> do
    logIt res $
      "Trying to see user reviews for: " <>
      T.pack (show userId) <> ", with: " <> T.pack (show spCrit) <> "."
    reviews <-
      runQuery res $ runSelectReturningList $ selectReviews spCrit userId
    reviewsCount' <-
      runQuery res $ runSelectReturningOne $ selectReviewsCount userId
    let reviewsCount = countPages spCrit reviewsCount'
    if null reviews
      then do
        logIt res ("No reviews found." :: T.Text)
        return $ TPaginated reviewsCount []
      else do
        let reviewAuthorKeys = _userWasReviewedBy <$> reviews
            authorToReview =
              M.fromList $
              fmap (\review -> (_userWasReviewedBy review, review)) reviews
        reviewAuthorGivenRanks <-
          runQuery res $
          runSelectReturningList $ selectRanksGivenByTo reviewAuthorKeys userId
        reviewAuthorsDetails <-
          runQuery res $
          runSelectReturningList $ selectUsersAndRanks reviewAuthorKeys
        let reviewAuthorToRank = M.fromList reviewAuthorGivenRanks
            reviewsConsed =
              traverse (consUserWithReview authorToReview reviewAuthorToRank) $
              (\(a, b, c) -> (a, round b, c)) <$> reviewAuthorsDetails
        case reviewsConsed of
          Nothing -> do
            logIt res ("Could not construct reviews." :: T.Text)
            throwError err500
          Just reviews' -> do
            logIt res ("Successfully constructed the reviews." :: T.Text)
            return $ TPaginated reviewsCount reviews'

filterTransactions ::
     WithSortPage TransactionOrdering (TPrefValue -> Handler (TPaginated TTransactionWithDetails))
filterTransactions res =
  withSPcrit (_maxPageSize res) TransByCreated $ \spCrit (TPrefValue sellOrBuy filterOpts nameFilter' descFilter') -> do
    logIt res $
      "Trying to filter transactions by: " <>
      (TL.toStrict . TA.encodeToLazyText) filterOpts <>
      ", with sellingOrBuying: " <>
      T.pack (show sellOrBuy) <>
      ", with pagination: " <> T.pack (show spCrit) <> "."
    transactions <-
      runQuery res $
      runSelectReturningList $
      selectFilterTransactions spCrit filterOpts nameFilter' descFilter'
    transactionsCount' <-
      runQuery res $
      runSelectReturningOne $
      selectFilterTransactionsCount filterOpts nameFilter' descFilter'
    let transactionsCount = countPages spCrit transactionsCount'
    logIt res ("Constructing transaction details..." :: T.Text)
    transactionsDetails <- constructTransactionsDetails res transactions
    return $ TPaginated transactionsCount transactionsDetails

listTransactions ::
     WithSortPage TransactionOrdering (Handler (TPaginated TTransactionWithDetails))
listTransactions res =
  withSPcrit (_maxPageSize res) TransByCreated $ \spCrit -> do
    logIt res $
      "Trying to list transactions with pagination: " <>
      T.pack (show spCrit) <> "."
    transactions <-
      runQuery res $ runSelectReturningList $ selectTransactions spCrit
    logIt res ("Constructing transaction details..." :: T.Text)
    transactionsCount' <-
      runQuery res $ runSelectReturningOne selectTransactionsCount
    let transactionsCount = countPages spCrit transactionsCount'
    transactionsDetails <- constructTransactionsDetails res transactions
    return $ TPaginated transactionsCount transactionsDetails

getTransactionDetails :: ByIdHandler TransactionId TTransactionWithDetails
getTransactionDetails res transactionId' = do
  logIt res $
    "Trying to obtain transaction details for: " <>
    T.pack (show transactionId') <> "."
  record <-
    runQuery res $
    runSelectReturningOne $ selectTransactionDetails [transactionId']
  case record of
    Nothing -> do
      logIt res ("Transaction not found." :: T.Text)
      throwError err404
    Just entity -> do
      logIt res ("Constructing transaction details..." :: T.Text)
      head <$> constructTransactionsDetails res [entity]
