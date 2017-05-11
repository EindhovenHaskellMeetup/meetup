-- | Exercises for Chapter 6.
--
-- Implement the functions in this module using recursion.

module Chapter06 (and, concat, replicate, elem, (!!), merge, msort) where

import           Prelude hiding (and, concat, elem, replicate, (!!))

-- * Exercise 1

-- | Decide if all logical values in a list are true.
and :: [Bool] -> Bool
and = undefined

-- | Concatenate a list of lists.
concat :: [[a]] -> [a]
concat = undefined

-- | Produce a list with identical @n@ elements.
replicate :: Int -> a -> [a]
replicate = undefined

-- | Select the nth element of a list.
(!!) :: [a] -> Int -> a
xs !! n = undefined
-- Note that you can also define @!!@ using prefix notation:
--
-- > (!!) xs n = ...

-- | Decide if a value is an element of a list.
elem :: Eq a => a -> [a] -> Bool
elem = undefined

-- * Exercise 2

-- | Merges two sorted lists of values to give a single sorted list. If any of
-- the two given lists are not sorted then the behavior is undefined.
merge :: Ord a => [a] -> [a] -> [a]
-- Remember to define @merge@ using recursion.
merge = undefined

-- * Exercise 3
msort :: Ord a => [a] -> [a]
msort = undefined

