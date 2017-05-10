-- | Exercises for chapter 7.

module Chapter07 (comprehension, map') where

-- * Exercise 1

answer::[Char]
answer = "curried functions"

-- * Exercise 2

comprehension :: (a -> b) -> (a -> Bool) -> [a] -> [b]
comprehension f p = (map f) . (filter p)

-- * Exercise 3
map' :: ((a -> b) -> [a] -> [b])
map' f = foldr ((:) . f) []