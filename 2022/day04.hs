import qualified Data.Set as Set
import Data.List
import System.IO
import Lib.Util

-- "2-4, 5-8" -> [["2-4", "5-8"], ...] 
parse xs = map (split ',') xs

-- "["1","5"] -> [1..5]
getRange (x:y:[]) = [(toInt x)..(toInt y)]

-- ["2-4", "5-8"] -> [[2..4], [5..8]]
parseRange (x:y:[]) = [Set.fromList rg1, Set.fromList rg2] 
  where fstRange = split '-' x
        sndRange = split '-' y
        rg1      = getRange fstRange
        rg2      = getRange sndRange

mapRange xs = map parseRange xs

xiba [] = []
xiba ((x:y:[]):xs) = r:(xiba xs)
  where s1 = Set.size x
        s2 = Set.size y
        s3 = Set.size (Set.intersection x y)
        o' = s3 == s1 || s3 == s2
        r  = if o' then 1 else 0

xibo [] = []
xibo ((x:y:[]):xs) = r:(xibo xs)
  where s1 = Set.size x
        s2 = Set.size y
        s3 = Set.size (Set.intersection x y)
        r  = if s3 > 0 then 1 else 0

-- just using composition 
f = sum . xiba . mapRange . parse
g = sum . xibo . mapRange . parse

main = do
    contents <- readFile "day04.input"
    let xs = (lines contents)

    putStrLn "solution 1"
    print (f xs)

    putStrLn "solution 2"
    print (g xs)
