{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Handlers.Inventory
  ( linkedProviders
  , inventoryForProvider
  , itemsForInventory
  , validateSteamOpenIdRequest
  ) where

import           Control.Monad.Extra                  (unlessM)
import qualified Data.Text                            as T
import           Database.Beam
import           Servant

import           App.Defs.Db
import           App.Defs.Enums
import           App.Defs.Handler
import           App.Defs.Logging
import           App.Models.UserAccount.Queries
import           App.Pagination
import           App.Resources
import           App.Transfer.Objects
import           Inventory.Handlers
import           Inventory.Models.AccountLink.Queries
import           Inventory.Providers
import           Inventory.Transfer.Objects

linkedProviders :: WithResourcesAuth (Handler [ProviderTypes])
linkedProviders res (_, email) = do
  logIt res $ "Attempting to see linked providers for: " <> email <> "."
  maybeUser <- runQuery res $ runSelectReturningOne $ selectUserByEmail email
  case maybeUser of
    Nothing -> do
      logIt res $ "Inconsistent user for: " <> email <> "."
      throwError err400
    Just user -> do
      links <-
        runQuery res $
        runSelectReturningList $ selectLinkedInventoriesByUserId (pk user)
      case traverse toSafeEnum links of
        Nothing -> do
          logIt res $
            "Inconsistent DB links for: " <>
            email <> ", with values: " <> T.pack (show links) <> "."
          throwError err500
        Just links' -> do
          logIt res ("Successfully fetched links." :: T.Text)
          return links'

inventoryForProvider ::
     WithSortPageAuthAlso ProviderTypes InventoryOrdering (Handler (TPaginated TInventory))
inventoryForProvider res (_, email) provider =
  withSPcrit (_maxPageSize res) InventoryAsIs $ \spCrit -> do
    logIt res $
      "Attempting to see inventories of: " <>
      email <> ", for provider: " <> T.pack (show provider) <> "."
    maybeUser <- runQuery res $ runSelectReturningOne $ selectUserByEmail email
    let providers = _providers res
    case maybeUser of
      Nothing -> do
        logIt res $ "Inconsistency when fetching user info: " <> email <> "."
        throwError err500
      Just user ->
        case provider of
          SteamProviderType -> do
            logIt res $
              "Fetching linked account payload for steam for: " <> email <> "."
            case _steamProvider providers of
              Nothing -> do
                logIt res $
                  "Attempted to see steam inventory list when Steam provider is not enabled, for: " <>
                  email <> "."
                throwError err400
              Just steamProvider ->
                fetchSteamInventory res user steamProvider spCrit

itemsForInventory ::
     WithSortPageAuthAlso2 ProviderTypes TInventoryId ItemsOrdering (Handler (TPaginated TItem))
itemsForInventory res (_, email) provider inventoryId' =
  withSPcrit (_maxPageSize res) ItemsAsIs $ \spCrit -> do
    logIt res $
      "Attempting to see inventory contents of: " <>
      email <>
      ", for provider: " <>
      T.pack (show provider) <> ", for inventory: " <> inventoryId' <> "."
    maybeUser <- runQuery res $ runSelectReturningOne $ selectUserByEmail email
    let providers = _providers res
    case maybeUser of
      Nothing -> do
        logIt res $ "Inconsistency when fetching user info: " <> email <> "."
        throwError err500
      Just user ->
        case provider of
          SteamProviderType -> do
            logIt res $
              "Fetching linked account payload for steam for: " <> email <> "."
            case _steamProvider providers of
              Nothing -> do
                logIt res $
                  "Attempted to see steam inventory list when Steam provider is not enabled, for: " <>
                  email <> "."
                throwError err400
              Just steamProvider -> do
                unlessM (hasSteamInventory res user steamProvider inventoryId') $ do
                  logIt res $
                    "Attempted to see steam inventory for inventoryId: " <>
                    inventoryId' <>
                    ", for: " <>
                    email <> ", when inventoryId does not belong to the user."
                  throwError err400
                fetchSteamInventoryContents res user inventoryId' spCrit

validateSteamOpenIdRequest ::
     WithResourcesAuth (TOpenIdData -> Handler NoContent)
validateSteamOpenIdRequest res (_, email) openIdData = do
  logIt res $ "Attempting to see linked providers for: " <> email <> "."
  if hasSteam (_providers res)
    then do
      maybeUser <-
        runQuery res $ runSelectReturningOne $ selectUserByEmail email
      case maybeUser of
        Nothing -> do
          logIt res $ "Inconsistent user for: " <> email <> "."
          throwError err400
        Just user -> do
          validateOpenIdRequest res user openIdData
          return NoContent
    else do
      logIt res $
        "Attempting to link account: " <>
        email <> ", when steam is not an enabled provider."
      throwError err400
