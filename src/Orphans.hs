{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Orphans
  (
  ) where

import           Control.Lens.Operators
import           Data.Aeson
import           Data.Int                                   (Int64)
import           Data.Proxy
import           Data.Swagger
import           Data.Text
import           Database.Beam.Backend.SQL.BeamExtensions   (SqlSerial (..))
import           Database.Beam.Migrate.Generics
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Pg
import           Network.HTTP.Types.Method                  (Method)
import           Servant.Auth.Server
import           Servant.Foreign

instance (ToJSON a, ToJWT a, ToJSON b, ToJWT b) => ToJWT (a, b)

instance (FromJSON a, FromJWT a, FromJSON b, FromJWT b) => FromJWT (a, b)

instance ToJWT Text

instance FromJWT Text

instance ToJSON a => ToJSON (PgJSONB a) where
  toJSON (PgJSONB a) = toJSON a

instance ToSchema a => ToSchema (SqlSerial a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy a)

instance ToSchema a => ToSchema (PgJSONB a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy a)

instance ToSchema Value where
  declareNamedSchema _ =
    return $ NamedSchema (Just "JSON Object") $ mempty & type_ ?~ SwaggerObject

instance HasDefaultSqlDataType Postgres (SqlSerial Int64) where
  defaultSqlDataType _ _ False = pgBigSerialType
  defaultSqlDataType _ _ True =
    PgDataTypeSyntax
      (PgDataTypeDescrOid (Pg.typoid Pg.int8) Nothing)
      (emit "BIGINT")
      (pgDataTypeJSON "bigint")

-- copy pasted from a relevant servant github issue.
instance forall lang ftype api a. ( HasForeign lang ftype api
                                  , HasForeignType lang ftype Text
         ) =>
         HasForeign lang ftype (Auth '[ JWT] a :> api) where
  type Foreign ftype (Auth '[ JWT] a :> api) = Foreign ftype api
  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) req
    where
      req = subR {_reqHeaders = HeaderArg arg : _reqHeaders subR}
      arg =
        Arg
          { _argName = PathSegment "Authorization"
          , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy Text)
          }

-- This is JUST for servant-options - and it works.
instance GenerateList NoContent (Method -> Req NoContent) where
  generateList _ = []
