{-# LANGUAGE DeriveGeneric #-}

module Inventory.Transfer.Objects
  ( TInventory(..)
  , TInventoryId
  , TItem(..)
  , TItemProp(..)
  , TItemProps
  , TOpenIdData(..)
  ) where

import           Data.Aeson
import           Data.Swagger
import qualified Data.Text    as T
import           GHC.Generics

type UrlLink = T.Text

type TInventoryId = T.Text

type TItemProps = [TItemProp]

data TInventory =
  TInventory
    { inventoryName :: T.Text
    , inventoryId   :: TInventoryId
    , inventoryLogo :: UrlLink
    }
  deriving (Eq, Show, Generic)

instance ToJSON TInventory

instance ToSchema TInventory

data TItem =
  TItem
    { itemName        :: T.Text
    , itemDescription :: T.Text
    , itemIconUrl     :: Maybe UrlLink
    , itemProps       :: TItemProps
    }
  deriving (Eq, Show, Generic)

instance ToJSON TItem

instance FromJSON TItem

instance ToSchema TItem

data TItemProp =
  TItemProp
    { itemPropName  :: T.Text
    , itemPropValue :: T.Text
    }
  deriving (Eq, Show, Generic)

instance ToJSON TItemProp

instance FromJSON TItemProp

instance ToSchema TItemProp

data TOpenIdData =
  TOpenIdData
    { ns             :: T.Text
    , mode           :: T.Text
    , op_endpoint    :: T.Text
    , claimed_id     :: T.Text
    , identity       :: T.Text
    , return_to      :: T.Text
    , response_nonce :: T.Text
    , assoc_handle   :: T.Text
    , signed         :: T.Text
    , sig            :: T.Text
    }
  deriving (Eq, Show, Generic)

instance FromJSON TOpenIdData

instance ToSchema TOpenIdData
