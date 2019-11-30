{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inventory.Providers.Steam
  ( listInventories
  , listInventoryContent
  ) where

import           Control.Monad.IO.Class                (MonadIO)
import           Data.Default.Class                    (def)
import           Data.String                           (fromString)
import qualified Data.Text                             as T
import           Network.HTTP.Simple                   (getResponseBody,
                                                        httpJSONEither, httpLBS)
import           Text.XML                              (parseLBS)
import           Text.XML.Cursor                       (fromDocument)

import           App.Defs.Logging
import           App.Resources
import           Inventory.Providers.Steam.ExtractJson
import           Inventory.Providers.Steam.ExtractXml
import           Inventory.Transfer.Objects

type SteamUserID64 = T.Text

type SteamAppId = T.Text

listInventoryContent ::
     MonadIO m => Resources -> SteamUserID64 -> SteamAppId -> m [TItem]
listInventoryContent res user appId = do
  let url =
        fromString . T.unpack $
        "https://steamcommunity.com/inventory/" <> user <> "/" <> appId <> "/2"
  responseJson <- getResponseBody <$> httpJSONEither url
  case responseJson of
    Left _ -> do
      logIt res $
        "Improper fetch of steam inventory contents for user: " <>
        user <> ", for inventory: " <> appId <> "."
      return []
    Right result -> do
      logIt res $
        "Fetched steam inventory contents for user: " <>
        user <> ", for inventory: " <> appId <> "."
      return $ obtainItems result

listInventories :: MonadIO m => Resources -> SteamUserID64 -> m [TInventory]
listInventories res user = do
  let url =
        fromString . T.unpack $
        "https://steamcommunity.com/profiles/" <> user <> "/games?tab=all&xml=1"
  responseBody <- getResponseBody <$> httpLBS url
  case parseLBS def responseBody of
    Left _ -> do
      logIt res $
        "Improper fetch of steam inventory data for user: " <> user <> "."
      return []
    Right doc -> do
      logIt res $ "Fetched proper inventory data for: " <> user <> "."
      return $ obtainInventoryRecords (fromDocument doc)
