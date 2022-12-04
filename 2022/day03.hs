import qualified Data.Set as Set
import Data.List
import System.IO
import Lib.Util

half :: String -> (String, String)
half xs = splitAt ((len xs) `div` 2) xs

chars = '.' : ['a'..'z'] ++ ['A'..'Z']

parseRugsack = toArray . half

parse xs = map parseRugsack xs

inter (x:y:[]) = x `intersect` y

mapInters = map inter

mapSet xs = map (Set.fromList) xs

backToList xs = map (Set.toList) xs

mapToChar = map toChar -- maybe you can use a concat here

count :: [Char] -> [Int]
count [] = []
count (x:xs) = z : (count xs)
  where r = (x `elemIndex` chars)
        z = case r of (Just i)  -> i
                      (Nothing) -> 0

-- coding the second part
prep [] = []
prep (x:y:z:xs) = (Set.toList i') : (prep xs)
  where x' = Set.fromList x
        y' = Set.fromList y
        z' = Set.fromList z 
        i  = Set.intersection x' y'
        i' = Set.intersection i z'


-- just using composition (this back and forth is ridiculous :facepalm) 
f = sum . count . mapToChar . backToList . mapSet . mapInters . parse
g = sum . count . mapToChar . prep

main = do
    contents <- readFile "day03.input"
    let xs = (lines contents)

    putStrLn "solution 1"
    print (f xs)

    putStrLn "solution 2"
    print (g xs)
