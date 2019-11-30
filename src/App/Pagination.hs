{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module App.Pagination
  ( SortPageSig
  , withSPcrit
  , sortOrderToOrder
  , countPages
  , SortOrder(..)
  , PaginatedApi
  , SortPageCriteria(..)
  ) where

import           Control.Lens.Operators
import           Data.Maybe                   (fromMaybe)
import           Data.Swagger
import           Database.Beam
import qualified Database.Beam.Backend.SQL
import qualified Database.Beam.Query.Internal
import           GHC.Generics                 (Generic)
import           Servant.API

countPages :: SortPageCriteria c -> Maybe Int -> Int
countPages _ Nothing = 1
countPages _ (Just 0) = 1
countPages spCrit (Just entityCount) =
  fromInteger $
  if mod' == 0
    then div'
    else div' + 1
  where
    div' = toInteger entityCount `div` _limit spCrit
    mod' = toInteger entityCount `mod` _limit spCrit

withSPcrit :: Integer -> c -> (SortPageCriteria c -> f) -> SortPageSig c f
withSPcrit maxPageSize defaultS cb sortCrit order number size = cb spCrit
  where
    spCrit = mkSortPageCriteria maxPageSize defaultS sortCrit order number size

type PaginatedApi order rest
   = QueryParam "sortBy" order :> QueryParam "order" SortOrder :> QueryParam "pageNumber" Integer :> QueryParam "pageSize" Integer :> rest

type SortPageSig s r
   = Maybe s -> Maybe SortOrder -> Maybe Integer -> Maybe Integer -> r

mkSortPageCriteria :: Integer -> c -> SortPageSig c (SortPageCriteria c)
mkSortPageCriteria maxPageSize defaultS s order pNumber pSize =
  SortPageCriteria sortCrit sortOrder offset limit
  where
    sortCrit = fromMaybe defaultS s
    sortOrder = fromMaybe Desc order
    pageNumber = fromMaybe 1 pNumber
    pageSize = max 1 $ min (fromMaybe maxPageSize pSize) maxPageSize
    offset = (pageNumber - 1) * pageSize
    limit = pageSize

data SortOrder
  = Asc
  | Desc
  deriving (Show, Generic)

sortOrderToOrder ::
     Database.Beam.Backend.SQL.BeamSqlBackend be
  => SortOrder
  -> QExpr be s a
  -> Database.Beam.Query.Internal.QOrd be s a
sortOrderToOrder Asc  = asc_
sortOrderToOrder Desc = desc_

instance ToParamSchema SortOrder where
  toParamSchema _ = mempty & type_ ?~ SwaggerString & enum_ ?~ ["asc", "desc"]

instance FromHttpApiData SortOrder where
  parseQueryParam "asc" = Right Asc
  parseQueryParam "desc" = Right Desc
  parseQueryParam x =
    Left $ "Sort Order (" <> x <> ") is not a recognized sorting order."

data SortPageCriteria criteria =
  SortPageCriteria
    { _sortBy    :: criteria
    , _sortOrder :: SortOrder
    , _offset    :: Integer
    , _limit     :: Integer
    }
  deriving (Show)
