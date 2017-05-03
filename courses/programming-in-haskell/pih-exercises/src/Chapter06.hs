-- | Exercises for Chapter 6.
--
-- Implement the functions in this module using recursion.

module Chapter06 (and, concat, replicate, elem, (!!), merge, msort) where

import           Prelude hiding (and, concat, elem, replicate, (!!))

-- * Exercise 1

-- | Decide if all logical values in a list are true.
and :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

-- | Concatenate a list of lists.
concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

-- | Produce a list with identical @n@ elements.
replicate :: Int -> a -> [a]
replicate n x
  | n <= 0 = []
  | otherwise = x:(replicate (n - 1) x)

-- | Select the nth element of a list.
(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n
  | n < 0 = error "Negative index"
  | otherwise = xs !! (n - 1)
_ !! _ = error "Index too large"

-- Note that you can also define @!!@ using prefix notation:
--
-- > (!!) xs n = ...

-- | Decide if a value is an element of a list.
elem :: Eq a => a -> [a] -> Bool
elem _ []     = False
elem x (y:ys) = x == y || elem x ys

-- * Exercise 2

-- | Merges two sorted lists of values to give a single sorted list. If any of
-- the two given lists are not sorted then the behavior is undefined.
merge :: Ord a => [a] -> [a] -> [a]
-- Remember to define @merge@ using recursion.
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) = if (x <= y)
                      then x:(merge xs (y:ys))
                      else y:(merge (x:xs) ys)

-- * Exercise 3
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort xs0) (msort xs1)
  where (xs0, xs1) = splitAt (length xs `div` 2) xs

