{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module App.Models.TransactionHist
  ( TransactionHistT(..)
  , TransactionHist
  , TransactionHistId
  , unTransactionHistKey
  ) where

import           Data.Aeson
import           Data.Int               (Int64)
import           Data.Time.LocalTime
import           Database.Beam

import           App.Models.UserAccount

data TransactionHistT f =
  TransactionHist
    { _transactionHistId        :: Columnar f Int64
    , _transactionHistAuthorId  :: PrimaryKey UserAccountT f
    , _transactionHistCreatedAt :: Columnar f LocalTime
    , _transactionHistClosedAt  :: Columnar f LocalTime
    }
  deriving (Generic, Beamable)

type TransactionHist = TransactionHistT Identity

deriving instance Show TransactionHist

deriving instance Eq TransactionHist

type TransactionHistId = PrimaryKey TransactionHistT Identity

deriving instance Show TransactionHistId

deriving instance Eq TransactionHistId

deriving instance Ord TransactionHistId

deriving instance ToJSON TransactionHistId

instance Table TransactionHistT where
  data PrimaryKey TransactionHistT
       f = TransactionHistId{unTransactionHistKey :: Columnar f Int64}
             deriving (Generic, Beamable)
  primaryKey = TransactionHistId . _transactionHistId
