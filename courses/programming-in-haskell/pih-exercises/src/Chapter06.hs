-- | Exercises for chapter 5.

module Chapter06 (and', concat', replicate', (!!!), elem', merge, msort) where

-- * Exercise 1
and' :: [Bool] -> Bool
and' [] = True
and' (False:xs) = False
and' (True:xs) = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xxs) = xs ++ concat' xxs

replicate' :: Int -> a -> [a]
replicate' n x | n == 0 = []
               | n > 0 = x:(replicate' (n-1) x)

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) n
    | n == 0 = x
    | n > 0 = (!!!) xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:xs) = (x == y) || (elem x xs)

-- * Exercise 2
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) | x < y = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort (xs) = merge (msort ys) (msort zs) where
                h = length xs `div` 2
                ys = take h xs
                zs = drop h xs