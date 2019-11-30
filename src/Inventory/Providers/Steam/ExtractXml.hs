{-# LANGUAGE OverloadedStrings #-}

module Inventory.Providers.Steam.ExtractXml
  ( obtainInventoryRecords
  ) where

import qualified Data.Map.Strict            as M
import           Data.Maybe                 (listToMaybe, mapMaybe)
import qualified Data.Text                  as T
import           Text.XML
import           Text.XML.Cursor

import           Inventory.Transfer.Objects

obtainInventoryRecords :: Cursor -> [TInventory]
obtainInventoryRecords cursor =
  let gamesCursors =
        child cursor >>= checkName ((== "games") . nameLocalName) >>= child >>=
        checkName ((== "game") . nameLocalName)
   in mapMaybe nodeInfoToInventory gamesCursors

nodeInfoToInventory :: Cursor -> Maybe TInventory
nodeInfoToInventory cursor = do
  let elements = mapMaybe (unNodeElement . node) $ child cursor
      elementPairs =
        (\el -> (nameLocalName . elementName $ el, elementNodes el)) <$>
        elements
      elementMap = M.fromList elementPairs
  appId <- elementMap M.!? "appID" >>= listToMaybe >>= unNodeContent
  name <- elementMap M.!? "name" >>= listToMaybe >>= unNodeContent
  logo <- elementMap M.!? "logo" >>= listToMaybe >>= unNodeContent
  return $ TInventory name appId logo

unNodeElement :: Node -> Maybe Element
unNodeElement (NodeElement e) = Just e
unNodeElement _               = Nothing

unNodeContent :: Node -> Maybe T.Text
unNodeContent (NodeContent c) = Just c
unNodeContent _               = Nothing
