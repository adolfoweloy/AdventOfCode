module Lib.Util
(toInt
) where

toInt :: [Char] -> Int
toInt num = read num :: Int
