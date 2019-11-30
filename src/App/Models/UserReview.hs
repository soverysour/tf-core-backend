{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module App.Models.UserReview
  ( UserReviewT(..)
  , UserReview
  , UserReviewId
  , ReviewOrdering(..)
  , unReviewKey
  ) where

import           Control.Lens.Operators
import           Data.Int                                 (Int64)
import           Data.Swagger
import           Data.Text
import           Data.Time.LocalTime
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions (SqlSerial (..))
import           Servant

import           App.Models.UserAccount
import           Orphans                                  ()

data UserReviewT f =
  UserReview
    { _userReviewId       :: Columnar f (SqlSerial Int64)
    , _userWasReviewedBy  :: PrimaryKey UserAccountT f
    , _userWhoWasReviewed :: PrimaryKey UserAccountT f
    , _userReviewValue    :: Columnar f Text
    , _userReviewedAt     :: Columnar f LocalTime
    }
  deriving (Generic, Beamable)

type UserReview = UserReviewT Identity

deriving instance Show UserReview

deriving instance Eq UserReview

type UserReviewId = PrimaryKey UserReviewT Identity

deriving instance Show UserReviewId

deriving instance Eq UserReviewId

instance Table UserReviewT where
  data PrimaryKey UserReviewT f = UserReviewId{unReviewKey ::
                                             Columnar f (SqlSerial Int64)}
                                  deriving (Generic, Beamable)
  primaryKey = UserReviewId . _userReviewId

data ReviewOrdering =
  ReviewByCreated
  deriving (Show)

instance FromHttpApiData ReviewOrdering where
  parseQueryParam "created" = Right ReviewByCreated
  parseQueryParam c =
    Left $ "Such an order criteria (" <> c <> ") does not exist."

instance ToSchema UserReview

instance ToParamSchema ReviewOrdering where
  toParamSchema _ = mempty & type_ ?~ SwaggerString & enum_ ?~ ["created"]
