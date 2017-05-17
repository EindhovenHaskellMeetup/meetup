{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Chapter09
import           Criterion.Main
import           Formatting
import           Formatting.Clock
import           System.Clock

timeIt :: IO a -> IO ()
timeIt act = do
  startT <- getTime Monotonic
  _<- act
  endT <- getTime Monotonic
  fprint (timeSpecs % "\n") startT endT

main :: IO ()
main = do
  timeIt $ print $ length $ solutions [1, 3, 7, 10, 25, 50] 765
  timeIt $ print $ length $ solutions' [1, 3, 7, 10, 25, 50] 765
  timeIt $ print $ length $ solutions'' [1, 3, 7, 10, 25, 50] 765
  -- This will take a huge amount of time:
  --  timeIt $ print $ length $ solutions'' [1, 3, 7, 10, 25, 50, 83] 765

