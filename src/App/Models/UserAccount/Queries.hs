{-# LANGUAGE TypeApplications #-}

module App.Models.UserAccount.Queries
  ( selectUsersAndRanks
  , selectUserByEmail
  , selectUserById
  , module App.Models.UserAccount
  ) where

import           Data.Text
import           Database.Beam
import           Database.Beam.Postgres

import           App.Models
import           App.Models.UserAccount
import           App.Models.UserRank

selectUsersAndRanks ::
     [UserAccountId] -> SqlSelect Postgres (UserAccountId, Double, UserAccount)
selectUsersAndRanks userIds =
  select $
  aggregate_
    (\(u, r) ->
       ( group_ $ pk u
       , as_ @Double $
         fromMaybe_ (val_ 0) $ avg_ $ fromMaybe_ (val_ 0) (_userRankValue r)
       , group_ u)) $
  filter_
    (\(user, _) ->
       unAccountKey (pk user) `in_` (val_ . unAccountKey <$> userIds)) $ do
    user <- all_ (_userAccounts tradeForallDb)
    rank <-
      leftJoin_
        (all_ (_userRanks tradeForallDb))
        (\rank -> _userWhoWasRanked rank ==. pk user)
    return (user, rank)

type Email = Text

selectUserByEmail :: Email -> SqlSelect Postgres UserAccount
selectUserByEmail emailAddr =
  select $
  filter_ (\user -> _userEmail user ==. val_ emailAddr) $
  all_ (_userAccounts tradeForallDb)

selectUserById :: UserAccountId -> SqlSelect Postgres UserAccount
selectUserById userId =
  select $
  filter_ (\user -> _userId user ==. (val_ . unAccountKey) userId) $
  all_ (_userAccounts tradeForallDb)
