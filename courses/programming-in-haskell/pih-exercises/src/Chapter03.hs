-- | Exercises for chapter 3.

module Chapter03 (
  xs0, xs1, xs2, xs3, t0, t1, twice, palindrome, double, pair, swap, second
  ) where

-- * Exercise 1

xs0 :: [Int]
xs0 = [0, 1]

-- Define the values below and give them types.

-- | TODO: define as @['a','b','c']@
xs1 = undefined

-- | TODO: define as @('a','b','c')@
t0 = undefined

-- | TODO: define as @[(False,'0'),(True,'1')]@
xs2 = undefined

-- | TODO: define as @([False,True],['0','1'])@
t1 = undefined

-- | TODO: define as @[tail,init,reverse]@
xs3 = undefined

-- * Exercise 2

-- Define the values below and give them types.

-- | Returns the second element of a list.
--
-- TODO: give it a type and implement.
second xs = undefined

-- | Swap the elements of a tuple.
--
-- TODO: give it a type and implement.
swap (x,y) = undefined

-- | Constructs a pair with the given elements, in the order in which the
-- parameters appear.
--
-- TODO: give it a type and implement.
pair x y = undefined

-- | Multiplies the given argument by 2.
--
-- TODO: give it a type and implement.
double x = undefined

-- | Determine wether the given list is a palindrome.
--
-- TODO: give it a type and implement.
palindrome xs = undefined

-- | Apply the given function twice to the given argument.
--
-- TODO: give it a type and implement.
twice f x = undefined
