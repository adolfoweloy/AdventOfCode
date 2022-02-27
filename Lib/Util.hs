module Lib.Util
( splitOn
, flatMap
) where

matcher :: Eq a => a -> a -> Bool
matcher x y = x == y

-- generic split that can be applied to any kind of list (strings is the most common usage)
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = case dropWhile (matcher x) xs of
  [] -> []
  s' -> x':(splitOn x xs')
    where (x',xs') = break (matcher x) s'


flatMap :: [[a]] -> [a]
flatMap []     = []
flatMap (xs:xs') = xs ++ (flatMap xs')
