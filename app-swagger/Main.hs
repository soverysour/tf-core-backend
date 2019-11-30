{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import           Data.Aeson
import           Data.Aeson.Text
import           Data.Proxy
import qualified Data.Text.IO         as T
import qualified Data.Text.Lazy       as T
import           Servant.Auth.Swagger ()
import           Servant.Swagger

import           App.Api
import           Orphans              ()

main :: IO ()
main = T.writeFile "out/swagger.json" apiJsonText
  where
    apiJsonText =
      T.toStrict . encodeToLazyText . toJSON $ toSwagger (Proxy @Api)
