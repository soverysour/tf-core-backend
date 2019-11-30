{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Inventory.Providers.Steam.ExtractJson
  ( obtainItems
  ) where

import           Data.Aeson
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (intersperse)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import qualified Data.Vector                as V

import           Inventory.Transfer.Objects

obtainItems :: Value -> [TItem]
obtainItems (Object o) =
  fromMaybe [] $
  "descriptions" `HM.lookup` o >>= \case
    (Array arr) -> traverse extractItemInfo $ V.toList arr
    _ -> Just []
obtainItems _ = []

extractItemInfo :: Value -> Maybe TItem
extractItemInfo (Object o) =
  case "tradable" `HM.lookup` o of
    Just (Number n) ->
      if n == 0
        then Nothing
        else extractItemInfo' o
    _ -> Nothing
extractItemInfo _ = Nothing

extractItemInfo' :: HM.HashMap T.Text Value -> Maybe TItem
extractItemInfo' o = do
  iconUrl <- "icon_url" `HM.lookup` o >>= unString
  descriptions <-
    "descriptions" `HM.lookup` o >>= unList >>= traverse unDescription
  type' <- "type" `HM.lookup` o >>= unString
  name <- "name" `HM.lookup` o >>= unString
  props <- "tags" `HM.lookup` o >>= unList >>= traverse unTag
  let description = mconcat . intersperse "\n" $ type' : descriptions
      url = "https://cdn.steamcommunity.com/economy/image/" <> iconUrl
      props' = uncurry TItemProp <$> props
  return $ TItem name description (Just url) props'

unString :: Value -> Maybe T.Text
unString (String s) = Just s
unString _          = Nothing

unList :: Value -> Maybe [Value]
unList (Array arr) = Just $ V.toList arr
unList _           = Nothing

unDescription :: Value -> Maybe T.Text
unDescription (Object o) = "value" `HM.lookup` o >>= unString
unDescription _          = Nothing

unTag :: Value -> Maybe (T.Text, T.Text)
unTag (Object o) = do
  tagName <- "localized_category_name" `HM.lookup` o >>= unString
  tagValue <- "localized_tag_name" `HM.lookup` o >>= unString
  return (tagName, tagValue)
unTag _ = Nothing
