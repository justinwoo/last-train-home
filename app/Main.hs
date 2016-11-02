{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Generic
import Lib
import Types

data Main = Main
  { lat :: Float
  , lon :: Float
  } deriving (Generic, Show)
instance ParseRecord Main

main :: IO ()
main = do
  opts <- getRecord "last-train-home -- find my way home from given lat and lon coordinates."
  run $ Origin $ Coords (lat opts, lon opts)
