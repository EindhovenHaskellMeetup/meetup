-- | Specification for the exercises of Chapter 04.

module Chapter05Spec where

import Chapter05 (pyths, perfects, scalarproduct)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Exercise 1: pyths" $ do

    it "yields all pythagorean triples with numbers at most n" $ do
      pyths (4 :: Int) `shouldBe` []
      pyths (5 :: Int) `shouldBe` [(3,4,5),(4,3,5)]

  describe "Exercise 2: perfects" $ do

    it "yields all perfect numbers up to n" $ do
      perfects (1 :: Int) `shouldBe` []
      perfects (500 :: Int) `shouldBe` [6,28,496]
      perfects (496 :: Int) `shouldBe` [6,28,496]

  describe "Exercise 3: scalar product" $ do

    it "yields the scalar product of 2 lists" $ do
      scalarproduct ([]::[Int]) ([]::[Int]) `shouldBe` []
      scalarproduct ([1,2,3]::[Int]) ([4,5,6]::[Int]) `shouldBe` [4,10,18]
      scalarproduct ([1,2,3,4,5,6]::[Int]) ([4,5,6]::[Int]) `shouldBe` [4,10,18]
      scalarproduct ([1,2,3]::[Int]) ([4,5,6,1,2,3]::[Int]) `shouldBe` [4,10,18]
