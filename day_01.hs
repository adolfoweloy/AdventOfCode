import Data.List
import System.IO

increased :: (Integral a) => Bool -> a
increased pred = if pred then 1 else 0

-- count the number of times each new input increases in relation to the previous input
count :: (Integral a) => [a] -> a
count [] = 0
count (x:[]) = 0
count (x:xs) = increased (head xs > x) + count xs

-- count the increase of a window of three inputs compared to the previous window of three
three :: (Integral a) => [a] -> [a] -> a
three _ []          = 0
three [] (y:ys)     = three [y] ys 
three (x:xs) (y:ys) =
  if (length (x:xs)) == 3
    then (increased $ new > prev) + (three (xs ++ [y]) ys)
    else three (x:xs ++ [y]) ys
  where
    prev = sum $ x:xs
    new  = sum $ xs ++ [y]

main = do
    contents <- readFile "day_01.input"
    let xs = map (\x -> read x :: Int) (words contents)
    putStrLn "solution 1"
    print (count xs)
    putStrLn "solution 2"
    print (three [] xs)


