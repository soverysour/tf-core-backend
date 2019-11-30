{-# LANGUAGE OverloadedStrings #-}

module Inventory.Providers
  ( hasProviders
  , hasSteam
  , Providers(..)
  , ProviderTypes(..)
  , InventoryOrdering(..)
  , ItemsOrdering(..)
  , SteamProvider(..)
  ) where

import           Control.Lens.Operators
import           Data.Aeson
import           Data.Swagger
import           Servant.API

import           Inventory.Transfer.Objects

newtype Providers =
  Providers
    { _steamProvider :: Maybe SteamProvider
    }
  deriving (Eq, Show)

hasProviders :: Providers -> Bool
hasProviders (Providers Nothing) = False
hasProviders _                   = True

hasSteam :: Providers -> Bool
hasSteam (Providers Nothing) = False
hasSteam _                   = True

data InventoryOrdering
  = InventoryAsIs
  | InventoryAlphabetical
  deriving (Eq, Show)

data ItemsOrdering =
  ItemsAsIs
  deriving (Eq, Show)

data SteamProvider =
  SteamProvider
    { _extraSteamApps   :: [TInventory]
    , _useOnlyExtraApps :: Bool
    }
  deriving (Eq, Show)

data ProviderTypes =
  SteamProviderType
  deriving (Eq, Show, Ord, Enum, Bounded)

instance ToJSON ProviderTypes where
  toJSON SteamProviderType = String "steam"

instance ToSchema ProviderTypes where
  declareNamedSchema _ =
    return $
    NamedSchema (Just "ProviderType") $
    mempty & type_ ?~ SwaggerInteger & enum_ ?~ ["steam"]

instance ToParamSchema ProviderTypes where
  toParamSchema _ = mempty & type_ ?~ SwaggerString & enum_ ?~ ["steam"]

instance ToParamSchema InventoryOrdering where
  toParamSchema _ = mempty & type_ ?~ SwaggerString & enum_ ?~ ["asIs", "alpha"]

instance ToParamSchema ItemsOrdering where
  toParamSchema _ = mempty & type_ ?~ SwaggerString & enum_ ?~ ["asIs"]

instance FromHttpApiData ProviderTypes where
  parseQueryParam "steam" = Right SteamProviderType
  parseQueryParam c = Left $ "Such a provider (" <> c <> ") does not exist."

instance FromHttpApiData InventoryOrdering where
  parseQueryParam "asIs" = Right InventoryAsIs
  parseQueryParam "alpha" = Right InventoryAlphabetical
  parseQueryParam c =
    Left $ "Such an order criteria (" <> c <> ") does not exist."

instance FromHttpApiData ItemsOrdering where
  parseQueryParam "asIs" = Right ItemsAsIs
  parseQueryParam c =
    Left $ "Such an order criteria (" <> c <> ") does not exist."
