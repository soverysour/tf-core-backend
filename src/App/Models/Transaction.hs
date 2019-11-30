{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module App.Models.Transaction
  ( TransactionT(..)
  , Transaction
  , TransactionId
  , TransactionOrdering(..)
  , unTransactionKey
  ) where

import           Control.Lens.Operators
import           Data.Aeson
import           Data.Int                                 (Int64)
import           Data.Swagger
import           Data.Text
import           Data.Time.LocalTime
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions (SqlSerial (..))
import           Servant

import           App.Models.UserAccount

data TransactionT f =
  Transaction
    { _transactionId        :: Columnar f (SqlSerial Int64)
    , _transactionAuthorId  :: PrimaryKey UserAccountT f
    , _transactionCreatedAt :: Columnar f LocalTime
    }
  deriving (Generic, Beamable)

type Transaction = TransactionT Identity

deriving instance Show Transaction

deriving instance Eq Transaction

type TransactionId = PrimaryKey TransactionT Identity

deriving instance Show TransactionId

deriving instance Eq TransactionId

deriving instance Ord TransactionId

deriving instance ToJSON TransactionId

instance Table TransactionT where
  data PrimaryKey TransactionT f = TransactionId{unTransactionKey ::
                                               Columnar f (SqlSerial Int64)}
                                   deriving (Generic, Beamable)
  primaryKey = TransactionId . _transactionId

data TransactionOrdering =
  TransByCreated
  deriving (Show)

instance FromHttpApiData TransactionOrdering where
  parseQueryParam "created" = Right TransByCreated
  parseQueryParam c =
    Left $ "Such an order criteria (" <> c <> ") does not exist."

instance ToSchema TransactionId where
  declareNamedSchema _ =
    return $
    NamedSchema (Just "TransactionID") $ mempty & type_ ?~ SwaggerInteger

instance ToParamSchema TransactionId where
  toParamSchema _ = mempty & type_ ?~ SwaggerInteger

instance ToParamSchema TransactionOrdering where
  toParamSchema _ = mempty & type_ ?~ SwaggerString & enum_ ?~ ["created"]

instance FromHttpApiData TransactionId where
  parseQueryParam t =
    TransactionId . SqlSerial <$>
    (parseQueryParam :: Text -> Either Text Int64) t
