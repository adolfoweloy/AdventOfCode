module Lib.Util
(toInt,
 len,
 toArray,
 toChar,
 indexOf,
 split
) where

import qualified Data.List as List

toInt :: [Char] -> Int
toInt num = read num :: Int

len :: [a] -> Int
len = foldl (\acc x -> acc + 1) 0

toArray :: (a,a) -> [a]
toArray (x,y) = [x, y]

toChar :: String -> Char
toChar [] = ' '
toChar (x:_) = x 

indexOf x xs = case (List.elemIndex x xs) of (Just n) -> n
                                             Nothing  -> -1

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split x xs = fstPart : (split x sndPart)
  where fstPart = takeWhile (/= x) xs
        sndPart = drop 1 (dropWhile (/= x) xs)


