{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module App.Api
  ( Api
  , StaticApi
  , AuthApi
  , VisitorApi
  , TraderApi
  , InventoryApi
  , SteamApi
  ) where

import           Servant.API

import           App.Defs.Handler
import           App.Models.Transaction
import           App.Models.UserAccount
import           App.Models.UserReview
import           App.Pagination
import           App.Transfer.Objects
import           Inventory.Providers
import           Inventory.Transfer.Objects

type StaticApi = "static" :> Raw

type AuthApi
   = "auth" :> "register" :> ReqBody '[ JSON] TUserRegister :> Post '[ JSON] NoContent :<|> "auth" :> "login" :> ReqBody '[ JSON] TUserLogin :> Post '[ JSON] TAuthToken

type VisitorApi
   = "offers" :> PaginatedApi TransactionOrdering (GetPartialContent '[ JSON] (TPaginated TTransactionWithDetails)) :<|> "offers" :> Capture "id" TransactionId :> Get '[ JSON] TTransactionWithDetails :<|> "offers" :> "filter" :> PaginatedApi TransactionOrdering (ReqBody '[ JSON] TPrefValue :> Post '[ JSON] (TPaginated TTransactionWithDetails)) :<|> "users" :> Capture "id" UserAccountId :> Get '[ JSON] TUserDetails :<|> "users" :> Capture "id" UserAccountId :> "reviews" :> PaginatedApi ReviewOrdering (GetPartialContent '[ JSON] (TPaginated TReview))

type TraderApi
   = "offers" :> Capture "inventoryProvider" ProviderTypes :> ReqBody '[ JSON] TCreateOffer :> Post '[ JSON] TransactionId :<|> "offers" :> Capture "id" TransactionId :> Delete '[ JSON] NoContent :<|> "users" :> Capture "id" UserAccountId :> "rank" :> ReqBody '[ JSON] TRankCreate :> Post '[ JSON] NoContent :<|> "users" :> Capture "id" UserAccountId :> "review" :> ReqBody '[ JSON] TReviewCreate :> Post '[ JSON] NoContent

type InventoryApi
   = "inventory" :> Get '[ JSON] [ProviderTypes] :<|> "inventory" :> Capture "inventoryProvider" ProviderTypes :> PaginatedApi InventoryOrdering (GetPartialContent '[ JSON] (TPaginated TInventory)) :<|> "inventory" :> Capture "inventoryProvider" ProviderTypes :> Capture "inventoryId" TInventoryId :> PaginatedApi ItemsOrdering (GetPartialContent '[ JSON] (TPaginated TItem))

type SteamApi
   = "steam-openid" :> ReqBody '[ JSON] TOpenIdData :> Post '[ JSON] NoContent

type Api
   = StaticApi :<|> VisitorApi :<|> AuthApi :<|> Protected TraderApi :<|> Protected InventoryApi :<|> Protected SteamApi
