{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}

module App.Defs.Handler
  ( WithResources
  , WithResourcesAuth
  , WithSortPage
  , WithSortPageAlso
  , WithSortPageAuthAlso
  , WithSortPageAuthAlso2
  , ByIdHandler
  , ByAuthIdHandler
  , Protected
  ) where

import           Servant
import           Servant.Auth.Server

import           App.Models.UserAccount
import           App.Pagination
import           App.Resources

type WithResources r = Resources -> r

type WithResourcesAuth r = WithResources (UserJwtData -> r)

type WithSortPage s r = WithResources (SortPageSig s r)

type WithSortPageAlso a s r = WithResources (a -> SortPageSig s r)

type WithSortPageAuthAlso a s r
   = WithResources (UserJwtData -> a -> SortPageSig s r)

type WithSortPageAuthAlso2 a1 a2 s r
   = WithResources (UserJwtData -> a1 -> a2 -> SortPageSig s r)

type ByIdHandler id r = WithResources (id -> Handler r)

type ByAuthIdHandler id r = WithResourcesAuth (id -> Handler r)

type Protected api = Auth '[ JWT] UserJwtData :> api
