module Chapter07Spec where

import Chapter07 (answer, comprehension, map', filter', filter'')
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Exercise 1: What are higher-order functions that return functions as results better known as?" $ do

    it "has the correct answer" $ do
      answer `shouldBe` "curried functions"

  describe "Exercise 2: Express the comprehension [f x | x <- xs, p x] using map and filter" $ do

      it "returns the empty list if both lists are empty" $ do
          comprehension (*2) (>4) [1,2,3,4,5,6,7,8] `shouldBe` [10,12,14,16]

  describe "Exercise 3: Redefine map f and filter p using foldr" $ do

      describe "map f" $ do

        it "returns empty list for empty list" $ do
            map' (*2) [] `shouldBe` []

        it "applies f to each element in the list " $ do
            map' (*2) [1,2,3,4] `shouldBe` [2,4,6,8]

      describe "filter p" $ do

        it "returns empty list for empty list" $ do
            filter' (>4) [] `shouldBe` []
            filter'' (>4) [] `shouldBe` []

        it "returns only elements matching the predicate in the same order" $ do
            filter' (>4) [1,5,2,6,3,7,8] `shouldBe` [5,6,7,8]
            filter'' (>4) [1,5,2,6,3,7,8] `shouldBe` [5,6,7,8]

