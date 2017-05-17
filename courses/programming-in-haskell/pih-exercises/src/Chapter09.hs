-- |

module Chapter09 (choices, choices', eval, Expr (..), Op (..), perms, split) where

-- * Arithmetic operators

-- | The four arithmetic operations allowed in the game.
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = y < x
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div

-- * Numeric expressions

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val x) = show x
  show (App op x y) = brak x ++ show op ++ brak y
    where brak (Val y) = show y
          brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val x)     = [x]
values (App _ x y) = values x ++ values y

eval :: Expr -> [Int]
eval (Val n) = [n | 0 < n]
eval (App op x y) = [ apply op evX evY | evX <- eval x
                                       , evY <- eval y
                                       , valid op evX evY
                                       ]

-- * Combinatorial functions

-- | Return all the subsequences of a list.
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

-- | @interleave x xs@ returns all the possible lists that result from
-- inserting @x@ into some position in @xs@.
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = [x:y:ys] ++ map (y:) (interleave x ys)

-- | Return all the permutations of a list.
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = perms xs >>= interleave x

-- | Return all possible choices of a list, which are given by selecting zero
-- or more elements of a list in any given order.
choices :: [a] -> [[a]]
choices xs = subs xs >>= perms

-- * Formalizing the problem

solution :: Expr -> [Int] -> Int -> Bool
solution e xs t = eval e == [t] && values e `elem` choices xs

-- * Brute force solution
solutions :: [Int] -> Int -> Expr
solutions = undefined

-- | Get all the possibilities for splitting a list into two non-empty lists.
split :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = [([x], xs)]
               ++ [(x:ys, zs) | (ys, zs) <- split xs]

-- | Get all possible expressions that can be constructed with the given list
-- of integers.
exprs :: [Int] -> [Expr]
exprs = undefined

-- * Exercise done in class

-- | The implementation of @choices@ as discussed on today's session. It needs
-- to be tidied up, and optimized.
choices' :: [a] -> [[a]]
choices' [] = [[]]
choices' (x:xs) = ys ++ alternates x ys
  where ys = choices' xs

alternates :: a -> [[a]] -> [[a]]
alternates x xss = concat (map (alternate x) xss)

alternate :: a -> [a] -> [[a]]
alternate x xs = alternate' x [] xs
-- alternate x [] = [[x]]
-- alternate x (y:ys) =
--   [x:y:ys] ++ map (y:) (alternate x ys)

alternate' :: a -> [a] -> [a] -> [[a]]
alternate' x xs [] = [xs ++ [x]]
alternate' x xs (y:ys) =
  [xs ++ [x] ++ (y:ys)] ++ alternate' x (xs++[y]) ys

