module Lib.Util
(toInt,
 len,
 toArray,
 toChar
) where

toInt :: [Char] -> Int
toInt num = read num :: Int

len :: [a] -> Int
len = foldl (\acc x -> acc + 1) 0

toArray :: (a,a) -> [a]
toArray (x,y) = [x, y]

toChar :: String -> Char
toChar [] = ' '
toChar (x:_) = x 
