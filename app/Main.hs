{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.Text.IO             as T
import           System.Remote.Monitoring
import           Text.Toml

import           App                      (run)
import           Parse                    (AllConf (AllConf), mkConf)

main :: IO ()
main = do
  conf <- T.readFile ".env.toml"
  case parseTomlDoc "" conf of
    Left _ -> putStrLn "Invalid .env.toml file."
    Right doc ->
      case mkConf doc of
        Just (AllConf ekgPort appConf) -> do
          _ <- forkServer "localhost" ekgPort
          run appConf
        Nothing ->
          putStrLn
            "Improper .env.toml configuration - could not create the AppConf from it."
