{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Inventory.Handlers.Steam
  ( fetchSteamInventory
  , fetchSteamInventoryContents
  , hasSteamInventory
  , validateProviderUrls
  , validateOpenIdRequest
  ) where

import           Control.Monad                        (when)
import           Data.Maybe                           (mapMaybe)
import           Data.String                          (fromString)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import           Database.Beam
import           Network.HTTP.Base                    (urlEncodeVars)
import           Network.HTTP.Simple                  (getResponseBody, httpBS)
import           Servant
import           Text.RE.PCRE.Text

import           App.Defs.Db
import           App.Defs.Logging
import           App.Models
import           App.Models.UserAccount
import           App.Pagination
import           App.Resources
import           App.Transfer.Objects
import           Inventory.Handlers.Steam.Internal
import           Inventory.Models.AccountLink.Queries
import           Inventory.Providers
import qualified Inventory.Providers.Steam            as Steam
import           Inventory.Transfer.Objects

validateOpenIdRequest :: Resources -> UserAccount -> TOpenIdData -> Handler ()
validateOpenIdRequest res user openIdData = do
  let vars =
        [ ("openid.ns", T.unpack $ ns openIdData)
        , ("openid.mode", "check_authentication")
        , ("openid.op_endpoint", T.unpack $ op_endpoint openIdData)
        , ("openid.claimed_id", T.unpack $ claimed_id openIdData)
        , ("openid.identity", T.unpack $ identity openIdData)
        , ("openid.return_to", T.unpack $ return_to openIdData)
        , ("openid.response_nonce", T.unpack $ response_nonce openIdData)
        , ("openid.assoc_handle", T.unpack $ assoc_handle openIdData)
        , ("openid.signed", T.unpack $ signed openIdData)
        , ("openid.sig", T.unpack $ sig openIdData)
        ]
      url =
        fromString $
        "https://steamcommunity.com/openid/login?" ++ urlEncodeVars vars
  response <- T.decodeLatin1 . getResponseBody <$> httpBS url
  if validOpenIdResponse response
    then do
      logIt res $
        "Valid Steam OpenID authentication for: " <>
        show (unAccountKey (pk user)) <> "."
      case T.split (== '/') (identity openIdData) of
        [] -> do
          logIt res $
            "Improper state - could not extract steam ID from successful openID auth for: " <>
            show (unAccountKey (pk user)) <> "."
          throwError err500
        xs -> do
          let steamId = last xs
          logIt res $
            "Obtained steam id: " <>
            steamId <> ", for: " <> (T.pack . show . unAccountKey . pk) user
          runQuery res $
            runInsert $
            insert (_accountLinks tradeForallDb) $
            insertExpressions
              [ AccountLink
                  default_
                  (val_ . pk $ user)
                  (val_ . fromEnum $ SteamProviderType)
                  (val_ steamId)
                  currentTimestamp_
              ]
          return ()
    else do
      logIt res $
        "Invalid Steam OpenID authentication for: " <>
        show (unAccountKey (pk user)) <> "."
      throwError err400

validOpenIdResponse :: T.Text -> Bool
validOpenIdResponse response = anyMatches $ response *=~ [re|is_valid:true|]

hasSteamInventory ::
     Resources -> UserAccount -> SteamProvider -> TInventoryId -> Handler Bool
hasSteamInventory res user steamProvider inventoryId' = do
  inventories <- fetchSteamInventory' res user steamProvider
  return $ any ((== inventoryId') . inventoryId) inventories

validateProviderUrls :: [TItem] -> ProviderTypes -> Handler ()
validateProviderUrls items SteamProviderType =
  when (any (not . validUrl) (mapMaybe itemIconUrl items)) (throwError err400)

validUrl :: T.Text -> Bool
validUrl url = cond1 || cond2
  where
    cond1 = anyMatches $ url *=~ [re|^https?://cdn\.steamcommunity\.com|]
    cond2 = anyMatches $ url *=~ [re|^https?://steamcdn-a\.akamaihd\.net|]

fetchSteamInventory ::
     Resources
  -> UserAccount
  -> SteamProvider
  -> SortPageCriteria InventoryOrdering
  -> Handler (TPaginated TInventory)
fetchSteamInventory res user steamProvider spCrit = do
  inventories <- fetchSteamInventory' res user steamProvider
  return $ paginateInventories inventories spCrit

fetchSteamInventory' ::
     Resources -> UserAccount -> SteamProvider -> Handler [TInventory]
fetchSteamInventory' res user steamProvider = do
  inventories <-
    if _useOnlyExtraApps steamProvider
      then return []
      else do
        maybeAccountPayload <-
          runQuery res $
          runSelectReturningOne $
          selectLinkedInventoryForUserAndProvider
            (pk user)
            (fromEnum SteamProviderType)
        case maybeAccountPayload of
          Nothing -> do
            logIt res $
              "User: " <>
              (T.pack . show) (pk user) <>
              ", doesn't have: " <>
              T.pack (show SteamProviderType) <> " linked."
            throwError err400
          Just payload -> do
            logIt res $
              "Fetching Steam inventory list for: " <>
              (T.pack . show) (pk user) <> ", with payload: " <> payload <> "."
            Steam.listInventories res payload
  return (_extraSteamApps steamProvider ++ inventories)

fetchSteamInventoryContents ::
     Resources
  -> UserAccount
  -> TInventoryId
  -> SortPageCriteria ItemsOrdering
  -> Handler (TPaginated TItem)
fetchSteamInventoryContents res user inventoryId' spCrit = do
  maybeAccountPayload <-
    runQuery res $
    runSelectReturningOne $
    selectLinkedInventoryForUserAndProvider
      (pk user)
      (fromEnum SteamProviderType)
  case maybeAccountPayload of
    Nothing -> do
      logIt res $
        "User: " <>
        (T.pack . show) (pk user) <>
        ", doesn't have: " <> T.pack (show SteamProviderType) <> " linked."
      throwError err400
    Just payload -> do
      logIt res $
        "Fetching Steam inventory content for: " <>
        (T.pack . show) (pk user) <>
        ", with payload: " <>
        payload <> ", with inventoryId: " <> inventoryId' <> "."
      items <- Steam.listInventoryContent res payload inventoryId'
      return $ paginateItems items spCrit
