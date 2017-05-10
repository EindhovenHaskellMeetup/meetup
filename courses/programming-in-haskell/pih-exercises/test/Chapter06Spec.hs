-- | Specification for the exercises of Chapter 04.

module Chapter06Spec where

import Chapter06 as C6
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Exercise 1: define a function that" $ do

    describe "returns true iff all logical values in a list are true" $ do

        it "returns True for the empty list" $ do
          C6.and ([]:: [Bool]) `shouldBe` True

        it "returns True if only True is in the list" $ do
          C6.and [True] `shouldBe` True

        it "returns False if only False is in the list" $ do
          C6.and [False] `shouldBe` False

        it "returns False if any value is False" $ do
          C6.and [True, False, False] `shouldBe` False
          C6.and [False, True, True] `shouldBe` False

        it "returns True if all values are True" $ do
          C6.and [True, True, True ] `shouldBe` True

    describe "concatenates a list of lists" $ do

        it "returns empty list for the empty list" $ do
          C6.concat ([] :: [[Int]]) `shouldBe` []

        it "returns the list for a list of only one list" $ do
          C6.concat ([[1,2,3]] :: [[Int]]) `shouldBe` [1,2,3]

        it "returns the concatenation of the list in the list" $ do
          C6.concat ([[1,2,3], [4,5,6], [7,8,9]] :: [[Int]]) `shouldBe` [1,2,3,4,5,6,7,8,9]

    describe "produces a list with n identical elements" $ do

        it "returns empty list for n < 0" $ do
          C6.replicate 0 True `shouldBe` []
          C6.replicate 0 'a' `shouldBe` []
          C6.replicate (-10::Int) True `shouldBe` []

        it "returns a list with 1 element for n = 1" $ do
          C6.replicate 1 13 `shouldBe` [13]

        it "returns a list with n times the same element" $ do
          C6.replicate 3 'a' `shouldBe` "aaa"
          C6.replicate 2 (13::Int) `shouldBe` [13,13]

    describe "selects the nth element of a list" $ do

        it "returns the element if n=0 and there is only one element" $ do
           ['a'] C6.!! 0 `shouldBe` 'a'

        it "returns the element if nth element" $ do
          "abcd" C6.!! 2 `shouldBe` 'c'

    describe "decides if an element is in a list" $ do

        it "returns False if the list is empty" $ do
          C6.elem 'a' [] `shouldBe` False

        it "returns True if the element is in the list" $ do
          C6.elem 'a' "dcba" `shouldBe` True

        it "returns False if the element is not in the list" $ do
          C6.elem 'e' "dcba" `shouldBe` False

  describe "Exercise 2: define a recursive function that" $ do

    describe "merges two sorted lists of values to give a single sorted list" $ do

        it "returns the empty list if both lists are empty" $ do
          C6.merge ([]::[Int]) ([]::[Int]) `shouldBe` []

        it "returns the other list if one of both lists is empty" $ do
          C6.merge "foo" "" `shouldBe` "foo"
          C6.merge "" "foo" `shouldBe` "foo"

        it "merges the two lists" $ do
          C6.merge "foo" "abr" `shouldBe` "abfoor"

  describe "Exercise 3: define a recursive function that implements merge sort" $ do
        it "returns the empty list if the list is empty" $ do
          C6.msort ([]::[Int]) `shouldBe` []

        it "returns the list if the has length 1" $ do
          C6.msort "a" `shouldBe` "a"

        it "returns the sorted list" $ do
          C6.msort "the quick brown fox jumps over the lazy dog" `shouldBe` "        abcdeeefghhijklmnoooopqrrsttuuvwxyz"
