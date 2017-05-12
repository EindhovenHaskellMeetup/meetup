-- | Exercises for chapter 3.

module Chapter03 (
  xs0, xs1, xs2, xs3, t0, t1, twice, palindrome, double, pair, swap, second
  ) where

-- * Exercise 1

xs0 :: [Int]
xs0 = [0, 1]

-- Define the values below and give them types.

-- | TODO: define as @['a','b','c']@
xs1 :: [Char]
xs1 = ['a','b','c']

-- | TODO: define as @('a','b','c')@
t0 :: (Char, Char, Char)
t0 = ('a','b','c')

-- | TODO: define as @[(False,'0'),(True,'1')]@
xs2 :: [(Bool, Char)]
xs2 = [(False,'0'),(True,'1')]

-- | TODO: define as @([False,True],['0','1'])@
t1 :: ([Bool], [Char])
t1 = ([False,True],['0','1'])

-- | TODO: define as @[tail,init,reverse]@
xs3 :: [[a] -> [a]]
xs3 = [tail,init,reverse]

-- * Exercise 2

-- Define the values below and give them types.

-- | Returns the second element of a list.
--
-- TODO: give it a type and implement.
second :: [a] -> a
second = head . tail

-- | Swap the elements of a tuple.
--
-- TODO: give it a type and implement.
swap :: (a, b) -> (b, a)
swap (x,y) = (y, x)

-- | Constructs a pair with the given elements, in the order in which the
-- parameters appear.
--
-- TODO: give it a type and implement.
pair :: a -> b -> (a, b)
pair = (,)

-- | Multiplies the given argument by 2.
--
-- TODO: give it a type and implement.
double :: Num a => a -> a
double = (* 2)

-- | Determine wether the given list is a palindrome.
--
-- TODO: give it a type and implement.
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

-- | Apply the given function twice to the given argument.
--
-- TODO: give it a type and implement.
twice :: (a -> a) -> a -> a
twice f = f . f
