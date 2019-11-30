{-# LANGUAGE FlexibleContexts #-}

module App.Models.UserReview.Queries
  ( selectExistingReviews
  , selectReviews
  , selectReviewsCount
  , module App.Models.UserReview
  ) where

import           Database.Beam
import           Database.Beam.Postgres

import           App.Models
import           App.Models.UserAccount
import           App.Models.UserReview
import           App.Pagination

selectExistingReviews ::
     UserAccountId -> UserAccountId -> SqlSelect Postgres Int
selectExistingReviews userWhoReviewed userWhoWasReviewed =
  select $
  aggregate_ (const countAll_) $
  filter_
    (\r ->
       unAccountKey (_userWasReviewedBy r) ==.
       (val_ . unAccountKey) userWhoReviewed &&.
       unAccountKey (_userWhoWasReviewed r) ==.
       (val_ . unAccountKey) userWhoWasReviewed) $
  all_ (_userReviews tradeForallDb)

selectReviews ::
     SortPageCriteria ReviewOrdering
  -> UserAccountId
  -> SqlSelect Postgres UserReview
selectReviews (SortPageCriteria criteria order offset limit) userId =
  case criteria of
    ReviewByCreated -> selection (sortOrderToOrder order . _userReviewedAt)
  where
    selection o =
      select $
      limit_ limit $
      offset_ offset $
      orderBy_ o $
      filter_
        (\review ->
           unAccountKey (_userWhoWasReviewed review) ==.
           (val_ . unAccountKey) userId) $
      all_ (_userReviews tradeForallDb)

selectReviewsCount :: UserAccountId -> SqlSelect Postgres Int
selectReviewsCount userWhoWasReviewed =
  select $
  aggregate_ (const countAll_) $
  filter_
    (\r ->
       unAccountKey (_userWhoWasReviewed r) ==.
       (val_ . unAccountKey) userWhoWasReviewed) $
  all_ (_userReviews tradeForallDb)
