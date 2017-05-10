module Chapter07Spec where

import Chapter07 (comprehension)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Exercise 1: What are higher-order functions that return functions as results better known as?" $ do

    it "has no tests" $ do
      True `shouldBe` True

  describe "Exercise 2: Express the comprehension [f x | x <- xs, p x] using map and filter" $ do

      it "returns the empty list if both lists are empty" $ do
          comprehension (*2) (>4) [1,2,3,4,5,6,7,8] `shouldBe` [10,12,14,16]