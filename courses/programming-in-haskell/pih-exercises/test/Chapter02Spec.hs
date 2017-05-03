-- | Specification for the exercises of Chapter 2.

module Chapter02Spec where

import           Chapter02       (myInit, myInit', myLast, myLast')
import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (Property, property, (==>))

checkLastDef :: ([Int] -> Int) -> Property
checkLastDef lastImpl = property $ \xs -> not (null xs)
  ==> lastImpl xs == last xs

checkInitDef :: ([Int] -> [Int]) -> Property
checkInitDef initImpl =
  property $ \xs -> not (null xs) ==> initImpl xs == init xs

spec :: Spec
spec = do
  describe "myLast" $ do

    it "correctly implements last" $ checkLastDef myLast

  describe "myLast'" $ do

    it "correctly implements last" $ checkLastDef myLast'

  describe "myInit" $ do

    it "correctly implements init" $ checkInitDef myInit

  describe "myInit'" $ do

    it "correctly implements init" $ checkInitDef myInit'
