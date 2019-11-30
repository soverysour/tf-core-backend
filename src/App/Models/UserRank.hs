{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module App.Models.UserRank
  ( UserRankT(..)
  , UserRank
  , UserRankId
  , unRankKey
  ) where

import           Data.Int                                 (Int64)
import           Data.Swagger
import           Data.Time.LocalTime
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions (SqlSerial (..))

import           App.Models.UserAccount
import           Orphans                                  ()

data UserRankT f =
  UserRank
    { _userRankId       :: Columnar f (SqlSerial Int64)
    , _userWasRankedBy  :: PrimaryKey UserAccountT f
    , _userWhoWasRanked :: PrimaryKey UserAccountT f
    , _userRankValue    :: Columnar f Double
    , _userRankedAt     :: Columnar f LocalTime
    }
  deriving (Generic, Beamable)

type UserRank = UserRankT Identity

deriving instance Show UserRank

deriving instance Eq UserRank

type UserRankId = PrimaryKey UserRankT Identity

deriving instance Show UserRankId

deriving instance Eq UserRankId

instance Table UserRankT where
  data PrimaryKey UserRankT f = UserRankId{unRankKey ::
                                         Columnar f (SqlSerial Int64)}
                                deriving (Generic, Beamable)
  primaryKey = UserRankId . _userRankId

instance ToSchema UserRank
