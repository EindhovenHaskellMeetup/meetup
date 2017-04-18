{-# ScopedTypeVariables #-}
-- | Specification for the exercises of Chapter 03.

module Chapter03Spec where

import           Chapter03       (t0, t1, xs0, xs1, xs2, xs3)
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck (Property, property, (.&.), (===), (==>))

spec :: Spec
spec = do
  describe "Exercise 1" $ do

    it "implements xs0" $ do
      xs0 `shouldBe` [0, 1]

    it "implements xs1" $ do
      xs1 `shouldBe` ['a','b','c']

    it "implements xs2" $ do
      xs2 `shouldBe` [(False,'0'),(True,'1')]

    it "implements xs3" $ property $ \xs ->
          (not (null xs) ==> (xs3 !! 0) xs === tail (xs :: [Int]))
      .&. (not (null xs) ==> (xs3 !! 1) xs === init xs)
      .&. (xs3 !! 2) xs === reverse xs

    it "implements t0" $ do
      t0 `shouldBe` ('a','b','c')

    it "implements t1" $ do
      t1 `shouldBe` ([False,True],['0','1'])

