-- | Exercises for chapter 5.

module Chapter05 (pyths, perfects, scalarproduct) where

-- * Exercise 1
pyths :: Int -> [(Int,Int,Int)]
pyths n =  [(x,y,z)| x <- [1..n], y <- [1..n], z <- [x..n], x*x+y*y == z*z]

-- * Exercise 2
perfects :: Int -> [Int]
perfects n = [ m | m <- [1..n], sum [f | f <- [1..m-1], m `mod` f == 0] == m]

-- * Exercise 3
scalarproduct :: [Int] -> [Int] -> [Int]
scalarproduct xs ys = [ m*n | (m,n) <- zip xs ys]
