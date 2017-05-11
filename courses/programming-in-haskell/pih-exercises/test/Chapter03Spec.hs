module Chapter03Spec where

import           Chapter03       (double, pair, palindrome, second, swap, t0,
                                  t1, twice, xs0, xs1, xs2, xs3)
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

  describe "Exercise 2" $ do

    it "implements function second" $ property $ \xs ->
      1 < length xs ==> second xs === (head (tail xs) :: [String])

    it "implements function swap" $ property $ \x y ->
      swap (x, y) === ((y, x) :: ([Bool], String))

    it "implements function pair" $ property $ \x y ->
      pair x y === ((x, y) :: (Char, Int))

    it "implements function double" $ property $ \x ->
      double x === 2.0 * (x :: Double)

    it "implements function palindrome" $ property $ \xs ->
      palindrome xs === (reverse xs == (xs :: [Int]))

    it "implements function twice" $ do
      twice (+3) 8 `shouldBe` (8 + 3 + 3)
      twice (++" bar") "foo" `shouldBe` "foo bar bar"
