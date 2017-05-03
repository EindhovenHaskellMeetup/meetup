-- | Specification for the exercises of Chapter 6.

module Chapter06Spec where

import qualified Chapter06       as C6

import           Data.List       (sort)
import           Data.Proxy
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck (Arbitrary (..), Property, property, (.&.),
                                  (===), (==>))

checkEquivalence :: (Arbitrary a, Show a, Show b, Eq b)
                 => Proxy a -> (a -> b) -> (a -> b) -> Property
checkEquivalence _ f g = property $ \x -> f x === g x

newtype IncreasingList a = IL [a] deriving (Eq, Show)

newtype NonEmptyList a = NEL { list :: [a] } deriving (Eq, Show)

instance (Ord a, Arbitrary a) => Arbitrary (IncreasingList a) where
  arbitrary = IL . sort <$> arbitrary

instance (Arbitrary a) => Arbitrary (NonEmptyList a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    return $ NEL (x:xs)

checkIndex :: Int -> Property
checkIndex i = checkEquivalence (Proxy :: Proxy (NonEmptyList Double))
               (apply (!!) i) (apply (C6.!!) i)
  where apply f j (NEL xs) = f xs j'
          where j' = (abs j) `min` (length xs - 1)

spec :: Spec
spec = do
  describe "Exercise 1" $ do

    it "implements and" $
      checkEquivalence (Proxy :: Proxy [Bool]) and C6.and

    it "implements concat" $
      checkEquivalence (Proxy :: Proxy [String]) concat C6.concat

    it "implements replicate" $ property $ \i ->
      checkEquivalence (Proxy :: Proxy Char) (replicate i) (C6.replicate i)

    it "implements !!" $ property $ checkIndex

    it "implements elem" $ property $ \str ->
      checkEquivalence (Proxy :: Proxy [String]) (str `elem`) (str `C6.elem`)

  describe "Exercise 2" $ do

    it "merge returns a sorted list" $
      checkEquivalence (Proxy :: Proxy ([Double], [Double]))
      (uncurry C6.merge) (\(xs, ys) -> sort (xs ++ ys))

  describe "Exercise 3" $ do

    it "implements merge sort" $ do
      checkEquivalence (Proxy :: Proxy [[Int]]) sort C6.msort



