{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Inventory.Models.AccountLink
  ( AccountLinkT(..)
  , AccountLink
  , AccountLinkId
  , unAccountLinkKey
  ) where

import           Data.Int                                 (Int64)
import           Data.Text                                (Text)
import           Data.Time.LocalTime                      (LocalTime)
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions (SqlSerial (..))

import           App.Models.UserAccount

data AccountLinkT f =
  AccountLink
    { _accountLinkId           :: Columnar f (SqlSerial Int64)
    , _linkedUserId            :: PrimaryKey UserAccountT f
    , _linkedInventoryProvider :: Columnar f Int
    , _linkedAccountPayload    :: Columnar f Text
    , _accountLinkCreatedAt    :: Columnar f LocalTime
    }
  deriving (Generic, Beamable)

type AccountLink = AccountLinkT Identity

deriving instance Show AccountLink

deriving instance Eq AccountLink

type AccountLinkId = PrimaryKey AccountLinkT Identity

deriving instance Show AccountLinkId

deriving instance Eq AccountLinkId

instance Table AccountLinkT where
  data PrimaryKey AccountLinkT f = AccountLinkId{unAccountLinkKey ::
                                               Columnar f (SqlSerial Int64)}
                                   deriving (Generic, Beamable)
  primaryKey = AccountLinkId . _accountLinkId
