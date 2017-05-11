module Chapter05Spec where

import           Chapter05  (perfects, pyths, scalarproduct)
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Exercise 1: pyths" $ do

    it "yields all pythagorean triples with numbers at most n" $ do
      pyths 4 `shouldBe` []
      pyths 5 `shouldBe` [(3,4,5),(4,3,5)]

  describe "Exercise 2: perfects" $ do

    it "yields all perfect numbers up to n" $ do
      perfects 1 `shouldBe` []
      perfects 500 `shouldBe` [6,28,496]
      perfects 496 `shouldBe` [6,28,496]

  describe "Exercise 3: scalar product" $ do

    it "yields the scalar product of 2 lists" $ do
      scalarproduct [] [] `shouldBe` 0
      scalarproduct [1,2,3] [4,5,6] `shouldBe` 32
      scalarproduct [1,2,3,4,5,6] [4,5,6] `shouldBe` 32
      scalarproduct [1,2,3] [4,5,6,1,2,3] `shouldBe` 32
