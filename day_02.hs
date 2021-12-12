import Data.List
import System.IO

type Position   = (Int, Int)
type Increment  = Int
type Command    = (String, Increment)
type Aim        = Int
type Position'  = (Int, Int, Aim)

nextPos :: Command -> Position -> Position
nextPos (c, n) (h, d) = case c of
  "forward" -> (h + n, d)
  "down"    -> (h, d + n)
  "up"      -> (h, d - n)

nextPos' :: Command -> Position' -> Position'
nextPos' (c, x) (h, d, a) = case c of
  "forward" -> (h + x, d + (a * x), a)
  "down"    -> (h, d, a + x)
  "up"      -> (h, d, a - x)

dive :: [Command] -> Position -> Position
dive []  p     = p
dive [c] p     = nextPos c p 
dive (c:xs) p  = dive xs (nextPos c p)

dive' :: [Command] -> Position' -> Position'
dive' [] p      = p
dive' [c] p     = nextPos' c p
dive' (c:xs) p  = dive' xs (nextPos' c p)

toi :: [Char] -> Int
toi c = read c :: Int

parse :: [String] -> [Command]
parse []        = []
parse (x:y:xs)  = (x, toi y):(parse xs)

calc :: Position -> Int
calc (x,y) = x * y

calc' :: Position' -> Int
calc' (x,y,_) = x * y

main = do
    contents <- readFile "day_02.input"
    let xs = parse $ words contents
    putStrLn "solution 1"
    print $ calc $ dive xs (0, 0)

    putStrLn "solution 2"
    print $ calc' $ dive' xs (0, 0, 0)

