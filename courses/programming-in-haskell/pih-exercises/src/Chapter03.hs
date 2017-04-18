-- | Exercises for chapter 3.

module Chapter03 (xs0, xs1, xs2, xs3, t0, t1) where

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
