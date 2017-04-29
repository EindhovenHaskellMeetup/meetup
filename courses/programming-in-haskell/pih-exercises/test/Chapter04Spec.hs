-- | Specification for the exercises of Chapter 04.

module Chapter04Spec where

import           Chapter04       (safetail, safetail', safetail'', or', or'', or''', and', and'')
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck (Property, property, (.&.), (===), (==>))

spec :: Specs
spec = do
  describe "Exercise 1" $ do

    it "implements safetail using a conditional expression" $ do
      safetail [] `shouldBe` []
      safetail [1, 2] `shouldBe` [2]

    it "implements safetail using guarded equations" $ do
      safetail' [] `shouldBe` []
      safetail' [1, 2] `shouldBe` [2]

    it "implements safetail using pattern matching" $ do
      safetail'' [] `shouldBe` []
      safetail'' [1, 2] `shouldBe` [2]

  describe "Exercise 2" $ do

    it "implements the logical or operator (||) using pattern matching (definition 1)" $ do
      or' false false `shouldBe` false
      or' true true `shouldBe` true
      or' true false `shouldBe` true
      or' false true `shouldBe` true

    it "implements the logical or operator (||) using pattern matching (definition 2)" $ do
      or'' false false `shouldBe` false
      or'' true true `shouldBe` true
      or'' true false `shouldBe` true
      or'' false true `shouldBe` true

    it "implements the logical or operator (||) using pattern matching (definition 3)" $ do
      or''' false false `shouldBe` false
      or''' true true `shouldBe` true
      or''' true false `shouldBe` true
      or''' false true `shouldBe` true

  describe "Exercise 3" $ do

    it "implements the logical and operator (&&) using a conditional expression)" $ do
      and' false false `shouldBe` false
      and' true true `shouldBe` true
      and' true false `shouldBe` false
      and' false true `shouldBe` false

  describe "Exercise 4" $ do

    it "implements the alternative logical and operator (&&) using a conditional expression)" $ do
      and'' false false `shouldBe` false
      and'' true true `shouldBe` true
      and'' true false `shouldBe` false
      and'' false true `shouldBe` false