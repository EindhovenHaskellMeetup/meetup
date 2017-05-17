module Main where

import           Chapter09
import           Criterion.Main

main :: IO ()
main = defaultMain [
  bgroup "finding 780 solutions" [
      bench "brute force solution" (nf length $ solutions [1, 3, 7, 10, 25, 50] 765)
      ]
  ]
