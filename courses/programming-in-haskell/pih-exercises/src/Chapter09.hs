-- |

module Chapter09 (choices) where

-- | The implementation of @choices@ as discussed on today's session. It needs
-- to be tidied up, and optimized.
choices :: [a] -> [[a]]
choices [] = [[]]
choices (x:xs) = ys ++ alternates x ys
  where ys = choices xs

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

