import Data.List
import System.IO
import Data.Char(digitToInt)


type Counter      = Int
type Binary       = [Int]
type Accumulator  = [Int]

bin' :: Binary -> Counter -> Accumulator -> Int
bin' []     _ acc = sum acc
bin' (x:xs) i acc = bin' xs next newL
  where
    calc = x * (2 ^ i)
    next = i + 1
    newL = [calc] ++ acc

bin :: Binary -> Int
bin xs = bin' reversedList 0 []
  where
    reversedList = reverse xs
-----

convert :: String -> [Int]
convert xs = map (\x -> digitToInt x) xs


rate rSize rSum = if isOn then 1 else 0
  where 
    half  = div rSize 2
    isOn  = rSum > half


gammaRate :: Int -> [Int] -> [Int]
gammaRate rSize xs = map (\x -> (rate rSize x)) xs


sumVectors :: [Int] -> [Int] -> [Int] -> [Int]
sumVectors [] [] acc          = acc
sumVectors (x:xs) (y:ys) acc  = sumVectors xs ys (acc ++ [x + y])


scan :: [[Int]] -> [Int] -> [Int]
scan [] acc           = acc
scan (xs':xs) acc     = scan xs newAcc
  where 
    newAcc  = sumVectors xs' acc []


findGammaRate :: [[Int]] -> [Int]
findGammaRate xs = gammaRate (length xs) listCount
  where
    lbin      = length (head xs)
    listCount = scan xs (replicate lbin 0)


main = do
    contents <- readFile "day_03.input"
    let xs   = map convert (words contents)
    let lbin = length (head xs)

    putStrLn "solution 1"
    let gamma   = findGammaRate xs
    let epsilon = map (\x -> if x == 1 then 0 else 1) gamma
    print $ bin gamma * bin epsilon
    


