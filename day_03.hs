import Data.List
import System.IO
import Data.Char(digitToInt)

type Binary = [Int]
type Accumulator = [Int]
type Counter = Int

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

type Stats = (Int, Int)
type Report = [Int]

strToIntArray :: String -> Report
strToIntArray xs = map (\x -> digitToInt x) xs

-- functions to capture the frequency of 1s and 0s
frequency :: Report -> Stats -> Stats
frequency [] acc      = acc
frequency (x:xs) (on,off)
  | x == 1    = frequency xs (on+1, off)
  | otherwise = frequency xs (on, off+1)

statsIdentity :: Stats
statsIdentity = (0,0)

increment :: Stats -> Int -> Stats
increment (x,y) d = if d == 1 then (x+1,y) else (x,y+1)

stats :: Report -> Stats
stats xs = foldl increment statsIdentity xs

allStats :: [Report] -> [Stats]
allStats xs = map stats (transpose xs)


-- calculating bit criteria which returns which bit to consider valid for a given position in a binary number
data RatingType = Oxygen | CO2

bitCriteria :: Stats -> RatingType -> Int
bitCriteria (x,y) t = if x == y then result else maxMin
  where
    result = case t of
      Oxygen -> 1
      CO2    -> 0
    maxMin = case t of
      Oxygen -> if x > y then 1 else 0
      CO2    -> if x > y then 0 else 1


-- predicates for each rating type
pOxygen :: Int -> Report -> Stats -> Bool
pOxygen pos r s = bit == (bitCriteria s Oxygen)
  where bit = r !! pos

pCO2 :: Int -> Report -> Stats -> Bool
pCO2 pos r s = bit == (bitCriteria s CO2)
  where bit = r !! pos

filterOxygen xs sts  = filter' pOxygen 0 xs sts
filterCO2    xs sts  = filter' pCO2 0 xs sts

oxygenGenRate xs sts = bin $ filterOxygen xs sts
co2GenRate xs sts    = bin $ filterCO2 xs sts

filter' :: (Int -> Report -> Stats -> Bool) -> Int -> [Report] -> [Stats] -> Report
filter' _ _ [x] _ = x
filter' f pos rs (s:stats) = filter' f (pos+1) rs' stats
  where
    rs' = filter (\r -> (f pos r s)) rs

gammaRate stats   = bin $ map (\s -> if (fst s) > (snd s) then 1 else 0) stats
epsilonRate stats = bin $ map (\s -> if (fst s) < (snd s) then 1 else 0) stats
    

main = do
    contents <- readFile "day_03.input"
    let xs   = map strToIntArray (words contents)
    let lbin = length (head xs)
    let stats = allStats xs
    
    putStrLn "solution 1"
    print $ (gammaRate stats) * (epsilonRate stats)        
    putStrLn "solution 2"
    print $ (oxygenGenRate xs stats) * (co2GenRate xs stats) 

