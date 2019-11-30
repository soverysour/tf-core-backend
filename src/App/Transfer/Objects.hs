{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Transfer.Objects
  ( TUserLogin(..)
  , TUserRegister(..)
  , TAuthToken(..)
  , TCreateOffer(..)
  , TUserDetails(..)
  , TTransaction(..)
  , TTransactionWithDetails(..)
  , TPrefValue(..)
  , TRankCreate(..)
  , TReviewCreate(..)
  , TReview(..)
  , TPaginated(..)
  ) where

import           Data.Aeson
import           Data.Int                   (Int64)
import           Data.Swagger
import           Data.Text
import           Data.Time.LocalTime
import           GHC.Generics

import           Inventory.Transfer.Objects
import           Orphans                    ()

data TUserLogin =
  TUserLogin
    { logEmail    :: Text
    , logPassword :: Text
    }
  deriving (Eq, Show, Generic)

instance FromJSON TUserLogin

instance ToSchema TUserLogin

data TUserRegister =
  TUserRegister
    { regNickname :: Text
    , regEmail    :: Text
    , regPassword :: Text
    }
  deriving (Eq, Show, Generic)

instance FromJSON TUserRegister

instance ToSchema TUserRegister

newtype TAuthToken =
  TAuthToken
    { authToken :: Text
    }
  deriving (Eq, Show, Generic)

instance FromJSON TAuthToken

instance ToJSON TAuthToken

instance ToSchema TAuthToken

data TCreateOffer =
  TCreateOffer
    { getSelling :: [TItem]
    , getBuying  :: [TItem]
    }
  deriving (Eq, Show, Generic)

instance FromJSON TCreateOffer

instance ToSchema TCreateOffer

data TUserDetails =
  TUserDetails
    { getUserId   :: Int64
    , getNickname :: Text
    , getAvgRank  :: Int
    }
  deriving (Eq, Show, Generic)

instance ToJSON TUserDetails

instance ToSchema TUserDetails

data TTransaction =
  TTransaction
    { transactionId :: Int64
    , createdAt     :: LocalTime
    }
  deriving (Eq, Show, Generic)

instance ToJSON TTransaction

instance ToSchema TTransaction

data TTransactionWithDetails =
  TTransactionWithDetails
    { getTransaction :: TTransaction
    , getUserDetails :: TUserDetails
    , getWtsItems    :: [TItem]
    , getWtbItems    :: [TItem]
    }
  deriving (Eq, Show, Generic)

instance ToJSON TTransactionWithDetails

instance ToSchema TTransactionWithDetails

data TPrefValue =
  TPrefValue
    { sellingOrBuying :: Bool
    , filterVal       :: TItemProps
    , nameFilter      :: Text
    , descFilter      :: Text
    }
  deriving (Eq, Show, Generic)

instance FromJSON TPrefValue

instance ToSchema TPrefValue

newtype TRankCreate =
  TRankCreate
    { rankValue :: Int
    }
  deriving (Eq, Show, Generic)

instance FromJSON TRankCreate

instance ToSchema TRankCreate

newtype TReviewCreate =
  TReviewCreate
    { reviewText :: Text
    }
  deriving (Eq, Show, Generic)

instance FromJSON TReviewCreate

instance ToSchema TReviewCreate

data TReview =
  TReview
    { getReview              :: Text
    , getReviewAuthorDetails :: TUserDetails
    , getGivenRank           :: Maybe Int
    }
  deriving (Eq, Show, Generic)

instance ToJSON TReview

instance ToSchema TReview

data TPaginated a =
  TPaginated
    { pageCount :: Int
    , pageData  :: [a]
    }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (TPaginated a)

instance ToSchema a => ToSchema (TPaginated a)
