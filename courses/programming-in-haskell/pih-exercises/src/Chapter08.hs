module Chapter08 (Nat(..), mult, Expr(..), folde, Tree(..), complete) where

data Nat = Zero
         | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add = undefined

mult :: Nat -> Nat -> Nat
mult = undefined

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

folde :: (Int -> a) -> (a -> a-> a) -> (a -> a -> a) -> Expr -> a
folde = undefined

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)

complete :: Tree a -> Bool
complete = undefined
