module Inventory.Handlers.Steam.Internal
  ( paginateInventories
  , paginateItems
  ) where

import           Data.List                  (sortBy)

import           App.Pagination
import           App.Transfer.Objects
import           Inventory.Providers
import           Inventory.Transfer.Objects

paginateInventories ::
     [TInventory] -> SortPageCriteria InventoryOrdering -> TPaginated TInventory
paginateInventories inventories spCrit@(SortPageCriteria by order offset limit) =
  let pageCount' = countPages spCrit (Just $ length inventories)
      sortf inv1 inv2 = inventoryName inv1 `compare` inventoryName inv2
      sortf' inv1 inv2 =
        case order of
          Asc  -> sortf inv1 inv2
          Desc -> sortf inv2 inv1
      inventories' =
        case by of
          InventoryAsIs         -> inventories
          InventoryAlphabetical -> sortBy sortf' inventories
      inventories'' =
        take (fromInteger limit) . drop (fromInteger offset) $ inventories'
   in TPaginated pageCount' inventories''

paginateItems :: [TItem] -> SortPageCriteria ItemsOrdering -> TPaginated TItem
paginateItems items spCrit@(SortPageCriteria by _ offset limit) =
  let pageCount' = countPages spCrit (Just $ length items)
      items' =
        case by of
          ItemsAsIs -> items
      items'' = take (fromInteger limit) . drop (fromInteger offset) $ items'
   in TPaginated pageCount' items''
