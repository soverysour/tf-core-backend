{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module App.Models.Item
  ( ItemT(..)
  , Item
  , ItemId
  , unItemKey
  ) where

import           Data.Aeson
import           Data.Int                                 (Int64)
import           Data.Swagger
import qualified Data.Text                                as T
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions (SqlSerial (..))
import           Database.Beam.Postgres

import           App.Models.Transaction
import           Inventory.Transfer.Objects               (TItemProps)
import           Orphans                                  ()

type Link = T.Text

data ItemT f =
  Item
    { _itemId              :: Columnar f (SqlSerial Int64)
    , _itemTransactionId   :: PrimaryKey TransactionT f
    , _itemName            :: Columnar f T.Text
    , _itemDescription     :: Columnar f T.Text
    , _itemIconUrl         :: Columnar f (Maybe Link)
    , _itemSellingOrBuying :: Columnar f Bool
    , _itemProperties      :: Columnar f (PgJSONB TItemProps)
    }
  deriving (Generic, Beamable)

type Item = ItemT Identity

deriving instance Show Item

deriving instance Eq Item

deriving instance ToJSON Item

type ItemId = PrimaryKey ItemT Identity

deriving instance Show ItemId

deriving instance Eq ItemId

deriving instance ToJSON ItemId

instance Table ItemT where
  data PrimaryKey ItemT f = ItemId{unItemKey ::
                                 Columnar f (SqlSerial Int64)}
                            deriving (Generic, Beamable)
  primaryKey = ItemId . _itemId

instance ToSchema Item
