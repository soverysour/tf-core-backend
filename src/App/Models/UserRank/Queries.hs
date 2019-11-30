module App.Models.UserRank.Queries
  ( selectExistingRanking
  , selectRanksGivenByTo
  , module App.Models.UserRank
  ) where

import           Database.Beam
import           Database.Beam.Postgres

import           App.Models
import           App.Models.UserAccount
import           App.Models.UserRank

selectExistingRanking ::
     UserAccountId -> UserAccountId -> SqlSelect Postgres Int
selectExistingRanking userWhoRanked userWhoWasRanked =
  select $
  aggregate_ (const countAll_) $
  filter_
    (\r ->
       unAccountKey (_userWasRankedBy r) ==. (val_ . unAccountKey) userWhoRanked &&.
       unAccountKey (_userWhoWasRanked r) ==.
       (val_ . unAccountKey) userWhoWasRanked) $
  all_ (_userRanks tradeForallDb)

selectRanksGivenByTo ::
     [UserAccountId]
  -> UserAccountId
  -> SqlSelect Postgres (UserAccountId, Double)
selectRanksGivenByTo usersWhoRanked userWhoWasRanked =
  select $
  fmap (\r -> (_userWasRankedBy r, _userRankValue r)) $
  filter_
    (\r ->
       unAccountKey (_userWasRankedBy r) `in_`
       (val_ . unAccountKey <$> usersWhoRanked) &&.
       unAccountKey (_userWhoWasRanked r) ==.
       (val_ . unAccountKey) userWhoWasRanked) $
  all_ (_userRanks tradeForallDb)
