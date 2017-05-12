module Chapter08 (Nat(..), mult, Expr(..), folde, Tree(..), complete) where

data Nat = Zero
         | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ(add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add (mult m n) n


data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

folde :: (Int -> a) -> (a -> a-> a) -> (a -> a -> a) -> Expr -> a
folde valFn _ _ (Val n) = valFn n
folde valFn addFn mulFn (Add e1 e2) = addFn f1 f2 where
                                        f1 = folde valFn addFn mulFn e1
                                        f2 = folde valFn addFn mulFn e2
folde valFn addFn mulFn (Mul e1 e2) = mulFn f1 f2 where
                                        f1 = folde valFn addFn mulFn e1
                                        f2 = folde valFn addFn mulFn e2

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)

complete :: Tree a -> Bool
complete t = f t > 0

-- f t := 0 iff 't is incomplete' && 'size of t' iff 't is complete'
f :: Tree a -> Int
f (Leaf l) = 1
f (Node tl l tr)
        | fl == 0 = 0
        | fr == 0 = 0
        | fl /= fr = 0
        | otherwise = fl + 1 + fr
        where
        fl = f tl
        fr = f tr -- TODO: how to evaluate this only when fl == 0?