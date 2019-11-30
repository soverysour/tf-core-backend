{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module App.Models
  ( TradeForallDb(..)
  , tradeForallDb
  , tradeForallCheckedDb
  ) where

import           Database.Beam
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.Types
import           Database.Beam.Postgres

import           App.Models.Item
import           App.Models.Transaction
import           App.Models.TransactionHist
import           App.Models.UserAccount
import           App.Models.UserRank
import           App.Models.UserReview
import           Inventory.Models.AccountLink
import           Orphans                        ()

data TradeForallDb f =
  TradeForallDb
    { _userAccounts         :: f (TableEntity UserAccountT)
    , _userReviews          :: f (TableEntity UserReviewT)
    , _userRanks            :: f (TableEntity UserRankT)
    , _transactions         :: f (TableEntity TransactionT)
    , _transactions_history :: f (TableEntity TransactionHistT)
    , _items                :: f (TableEntity ItemT)
    , _items_history        :: f (TableEntity ItemT)
    , _accountLinks         :: f (TableEntity AccountLinkT)
    }
  deriving (Generic, Database Postgres)

tradeForallCheckedDb :: CheckedDatabaseSettings Postgres TradeForallDb
tradeForallCheckedDb = defaultMigratableDbSettings

tradeForallDb :: DatabaseSettings Postgres TradeForallDb
tradeForallDb = unCheckDatabase tradeForallCheckedDb
