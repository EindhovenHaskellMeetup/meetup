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

-- Reference problem:
refVals = [1, 3, 7, 10, 25, 50]
refTarget = 765

main :: IO ()
main = do
  criterionBench
  simpleBench
  where criterionBench =
          defaultMain [
            bgroup "solutions"
            [ bench "version 0" $ nf (solutions refVals) refTarget
            , bench "version 1" $ nf (solutions' refVals) refTarget
            , bench "version 2" $ nf (solutions'' refVals) refTarget
            ]
          ]
        simpleBench = do
           print "benchmarks version 0"
           timeIt $ print $ length $ solutions refVals refTarget
           print "benchmarks version 1"
           timeIt $ print $ length $ solutions' refVals refTarget
           print "benchmarks version 2"
           timeIt $ print $ length $ solutions'' refVals refTarget
           -- -- This will take a huge amount of time:
           -- timeIt $ print $ length $ solutions'' [1, 3, 7, 10, 25, 50, 83] 765

