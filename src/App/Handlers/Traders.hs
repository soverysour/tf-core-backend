{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Handlers.Traders
  ( transactionCreate
  , transactionClose
  , userRank
  , userReview
  ) where

import           Control.Monad                            (when)
import           Control.Monad.IO.Class                   (liftIO)
import           Data.Maybe                               (fromMaybe)
import qualified Data.Text                                as T
import           Data.Time.Clock
import           Database.Beam
import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExtensions
import           Database.Beam.Postgres
import           Servant

import           App.Defs.Db
import           App.Defs.Handler
import           App.Defs.Logging
import           App.Models
import           App.Models.Item.Queries
import           App.Models.Transaction.Queries
import           App.Models.TransactionHist.Queries
import           App.Models.UserAccount.Queries
import           App.Models.UserRank.Queries
import           App.Models.UserReview.Queries
import           App.Transfer.Objects
import           Inventory.Handlers
import           Inventory.Providers
import           Inventory.Transfer.Objects

transactionClose :: ByAuthIdHandler TransactionId NoContent
transactionClose res (_, email) transactionId' = do
  logIt res $
    "Trying to close transaction: " <>
    T.pack (show transactionId') <> ", by: " <> email <> "."
  maybeUser <- runQuery res $ runSelectReturningOne $ selectUserByEmail email
  maybeTransaction <-
    runQuery res $
    runSelectReturningOne $ selectTransactionDetails [transactionId']
  case maybeTransaction of
    Nothing -> do
      logIt res $
        "No such transaction exists: " <> T.pack (show transactionId') <> "."
      throwError err400
    Just transaction ->
      let transactionAuthorId = _transactionAuthorId transaction
          maybeCanClose = (== transactionAuthorId) <$> (pk <$> maybeUser)
          canClose = fromMaybe False maybeCanClose
       in if canClose
            then do
              logIt res $
                "Can close transaction: " <> T.pack (show transactionId') <> "."
              let transactionCreatedAt = _transactionCreatedAt transaction
              runQuery res $
                runInsert $
                insert (_transactions_history tradeForallDb) $
                insertExpressions
                  [ TransactionHist
                      ((val_ . BeamExtensions.unSerial . unTransactionKey)
                         transactionId')
                      (val_ transactionAuthorId)
                      (val_ transactionCreatedAt)
                      currentTimestamp_
                  ]
              runQuery res $
                runInsert $ insertTransactionItemsToHist transactionId'
              runQuery res $ runDelete $ deleteTransaction transactionId'
              logIt res $
                "Successfully closed transaction: " <>
                T.pack (show transactionId') <> "."
              return NoContent
            else do
              logIt res $
                "Can not close transaction: " <>
                T.pack (show transactionId') <> "."
              throwError err400

transactionCreate ::
     WithResourcesAuth (ProviderTypes -> TCreateOffer -> Handler TransactionId)
transactionCreate res (_, email) provider createOffer@(TCreateOffer wtsItems wtbItems) = do
  logIt res $
    "Creating a transaction: " <>
    T.pack (show createOffer) <>
    ", for: " <>
    T.pack (show email) <> ", for: " <> T.pack (show provider) <> "."
  validateProviderUrls wtsItems provider
  validateProviderUrls wtbItems provider
  maybeUser <- runQuery res $ runSelectReturningOne $ selectUserByEmail email
  case maybeUser of
    Nothing -> do
      logIt res $ "No user found for: " <> email <> ". Probably not a problem."
      throwError err500
    Just user -> do
      let userId = pk user
      results <-
        runQuery res $
        BeamExtensions.runInsertReturningList $
        insert (_transactions tradeForallDb) $
        insertExpressions [Transaction default_ (val_ userId) currentTimestamp_]
      case results of
        [transaction] -> do
          let transactionKey = pk transaction
          runQuery res $
            runInsert $
            insert (_items tradeForallDb) $
            insertExpressions $
            fmap
              (\item ->
                 Item
                   default_
                   (val_ transactionKey)
                   (val_ $ itemName item)
                   (val_ $ itemDescription item)
                   (val_ $ itemIconUrl item)
                   (val_ True)
                   (val_ . PgJSONB $ itemProps item))
              wtsItems ++
            fmap
              (\item ->
                 Item
                   default_
                   (val_ transactionKey)
                   (val_ $ itemName item)
                   (val_ $ itemDescription item)
                   (val_ $ itemIconUrl item)
                   (val_ False)
                   (val_ . PgJSONB $ itemProps item))
              wtbItems
          logIt res $
            "Successfully created transaction: " <>
            T.pack (show transactionKey) <> "."
          return transactionKey
        _ -> do
          logIt
            res
            ("Could not properly create the transaction. Abording" :: T.Text)
          throwError err500

userRank ::
     WithResourcesAuth (UserAccountId -> TRankCreate -> Handler NoContent)
userRank res (_, email) userToRankId (TRankCreate rank) = do
  logIt res $
    "Ranking user: " <>
    T.pack (show userToRankId) <>
    ", by: " <> email <> ", with: " <> T.pack (show rank) <> "."
  when (rank < 1 || rank > 10) (throwError err400)
  time' <- liftIO getCurrentTime
  maybeLoggedUser <-
    runQuery res $ runSelectReturningOne $ selectUserByEmail email
  maybeToRankUser <-
    runQuery res $ runSelectReturningOne $ selectUserById userToRankId
  case (maybeLoggedUser, maybeToRankUser) of
    (Just loggedUser, Just toRankUser) -> do
      let loggedUserId = pk loggedUser
          toRankUserId = pk toRankUser
      when (loggedUserId == toRankUserId) $ do
        logIt res ("Attempted to rank oneself." :: T.Text)
        throwError err400
      maybePreviousRanking <-
        runQuery res $
        runSelectReturningOne $
        selectExistingRanking (pk loggedUser) (pk toRankUser)
      case maybePreviousRanking of
        Just 0 -> do
          runQuery res $
            runInsert $
            insert (_userRanks tradeForallDb) $
            insertExpressions
              [ UserRank
                  default_
                  (val_ loggedUserId)
                  (val_ toRankUserId)
                  ((val_ . fromIntegral) rank)
                  currentTimestamp_
              ]
          logIt res $
            "Successfully ranked user at time': " <> T.pack (show time') <> "."
          return NoContent
        _ -> do
          logIt res ("User has already ranked the other user." :: T.Text)
          throwError err400
    _ -> do
      logIt res ("Could not fetch users to perform ranking" :: T.Text)
      throwError err400

userReview ::
     WithResourcesAuth (UserAccountId -> TReviewCreate -> Handler NoContent)
userReview res (_, email) userToReviewId (TReviewCreate review) = do
  logIt res $
    "Reviewing user: " <>
    T.pack (show userToReviewId) <>
    ", by: " <> email <> ", with: " <> T.pack (show review) <> "."
  time' <- liftIO getCurrentTime
  maybeLoggedUser <-
    runQuery res $ runSelectReturningOne $ selectUserByEmail email
  maybeToReviewUser <-
    runQuery res $ runSelectReturningOne $ selectUserById userToReviewId
  case (maybeLoggedUser, maybeToReviewUser) of
    (Just loggedUser, Just toReviewUser) -> do
      let loggedUserId = pk loggedUser
          toReviewUserId = pk toReviewUser
      when (loggedUserId == toReviewUserId) $ do
        logIt res ("Attempted to review oneself." :: T.Text)
        throwError err400
      maybePreviousReviewing <-
        runQuery res $
        runSelectReturningOne $
        selectExistingReviews (pk loggedUser) (pk toReviewUser)
      case maybePreviousReviewing of
        Just 0 -> do
          runQuery res $
            runInsert $
            insert (_userReviews tradeForallDb) $
            insertExpressions
              [ UserReview
                  default_
                  (val_ loggedUserId)
                  (val_ toReviewUserId)
                  (val_ review)
                  currentTimestamp_
              ]
          logIt res $
            "Successfully reviewed user at time': " <>
            T.pack (show time') <> "."
          return NoContent
        _ -> do
          logIt res ("User has already reviewed the other user." :: T.Text)
          throwError err400
    _ -> do
      logIt res ("Could not fetch users to perform reviewing." :: T.Text)
      throwError err400
