-- | Exercices for Chapter 2.

module Chapter02
  ( myLast
  , myLast'
  , myInit
  , myInit'
  )
where

-- | Selects the last element of a list.
--
-- TODO: Define it using some of the functions seen on Chapter 2: 'head',
-- 'tail', 'nth', 'take', 'drop', 'length', 'sum', 'product', 'reverse'.
myLast :: [a] -> a
myLast = head . reverse

-- | Selects the last element of a list. Alternative version.
--
-- TODO: give an alternative definition, using only the functions described
-- above.
myLast' :: [a] -> a
myLast' xs = xs !! (length xs - 1)

-- | Remove the last element of a list.
--
-- TODO: define the function using only any combination of: 'head', 'tail',
-- 'nth', 'take', 'drop', 'length', 'sum', 'product', 'reverse'.
myInit  :: [a] -> [a]
myInit = reverse . tail . reverse

-- | Remove the last element of a list. Alternative version.
--
-- TODO: give an alternative definition, using only the functions described
-- above.
myInit' :: [a] -> [a]
myInit' xs = take (length xs - 1) xs
