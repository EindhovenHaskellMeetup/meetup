-- | Exercises for chapter 7.

module Chapter07 (comprehension) where

-- * Exercise 1

-- ?

-- * Exercise 2

comprehension :: (a -> b) -> (a -> Bool) -> [a] -> [b]
comprehension f p = (map f) . (filter p)
