import Data.List
import System.IO
import Lib.Util

parse [] = []
parse xs = igroup : parse rest
  where sgroup = fst (span (/= "") xs)
        igroup = map toInt sgroup
        rest   = drop 1 (snd (span (/= "") xs))

summedCalories = foldl (\acc x -> (sum x) : acc) []

sortedCalories xs = reverse $ sort xs

-- just using composition 
f = (sortedCalories . summedCalories . parse)

main = do
    contents <- readFile "day01.input"
    let xs = (lines contents)

    putStrLn "solution 1"
    print $ take 1 (f xs)

    putStrLn "solution 2"
    print $ (sum . take 3) (f xs)
