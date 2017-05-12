module Chapter08Spec where

import Chapter08
import Test.Hspec (Spec, describe, it, shouldBe)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

ten :: Nat
ten = int2nat (10::Int)

eval :: Expr -> Int
eval = folde id (+) (*)

spec :: Spec
spec = do
  describe "Exercise 1: Using recursion and the function add" $ do

    describe "define a function that" $ do

      it "multiplies two natural numbers" $ do
        mult Zero Zero `shouldBe` Zero
        mult Zero ten `shouldBe` Zero
        mult ten Zero `shouldBe` Zero
        mult ten (int2nat (13::Int)) `shouldBe` int2nat (130::Int)

    describe "Exercise 2: Define a suitable function folde for expressions, and give a few examples of its use" $ do

      it "applies the supplied functions appropriately" $ do
        eval (Add (Val 2) (Mul (Val 3) (Val 4))) `shouldBe` 14

    describe "Exercise 2: Define a function that decides if a binary tree is complete" $ do

      it "returns True for a one-node tree"$ do
        complete (Leaf 'a') `shouldBe` True

      it "returns True for a complete tree"$ do
        complete (Node (Leaf 'a') 'a' (Leaf 'a')) `shouldBe` True
        complete (Node (Node (Leaf 'a') 'a' (Leaf 'a')) 'a' (Node (Leaf 'a') 'a' (Leaf 'a'))) `shouldBe` True

      it "returns False for an incomplete tree"$ do
        complete (Node (Node (Leaf 'a') 'a' (Leaf 'a')) 'a' (Leaf 'a')) `shouldBe` False
        complete (Node (Leaf 'a') 'a' (Node (Leaf 'a') 'a' (Leaf 'a'))) `shouldBe` False