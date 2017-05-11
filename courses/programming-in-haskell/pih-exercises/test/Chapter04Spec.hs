module Chapter04Spec where

import           Chapter04       (safetail, safetail', safetail'', or', or'', or''', and', and'')
import           Test.Hspec      (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Exercise 1" $ do

    it "implements safetail using a conditional expression" $ do
      safetail ([] :: [Int]) `shouldBe` ([] :: [Int])
      safetail [1, 2 :: Int] `shouldBe` [2 :: Int]

    it "implements safetail using guarded equations" $ do
      safetail' ([] :: [Int]) `shouldBe` ([] :: [Int])
      safetail' [1, 2 :: Int] `shouldBe` [2 :: Int]

    it "implements safetail using pattern matching" $ do
      safetail'' ([] :: [Int]) `shouldBe` ([] :: [Int])
      safetail'' [1, 2 :: Int] `shouldBe` [2 :: Int]

  describe "Exercise 2" $ do

    it "implements the logical or operator (||) using pattern matching (definition 1)" $ do
      or' False False `shouldBe` False
      or' True True `shouldBe` True
      or' True False `shouldBe` True
      or' False True `shouldBe` True

    it "implements the logical or operator (||) using pattern matching (definition 2)" $ do
      or'' False False `shouldBe` False
      or'' True True `shouldBe` True
      or'' True False `shouldBe` True
      or'' False True `shouldBe` True

    it "implements the logical or operator (||) using pattern matching (definition 3)" $ do
      or''' False False `shouldBe` False
      or''' True True `shouldBe` True
      or''' True False `shouldBe` True
      or''' False True `shouldBe` True

  describe "Exercise 3" $ do

    it "implements the logical and operator (&&) using a conditional expression)" $ do
      and' False False `shouldBe` False
      and' True True `shouldBe` True
      and' True False `shouldBe` False
      and' False True `shouldBe` False

  describe "Exercise 4" $ do

    it "implements the alternative logical and operator (&&) using a conditional expression)" $ do
      and'' False False `shouldBe` False
      and'' True True `shouldBe` True
      and'' True False `shouldBe` False
      and'' False True `shouldBe` False