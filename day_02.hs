import Data.List
import System.IO

type Aim        = Int
type Position   = (Int, Int, Aim)
type Command    = (String, Int)
type NextPos    = (Command -> Position -> Position)

nextPos :: NextPos 
nextPos (c, n) (h, d, _) = case c of
  "forward" -> (h + n, d, 0)
  "down"    -> (h, d + n, 0)
  "up"      -> (h, d - n, 0)

nextPos' :: NextPos 
nextPos' (c, x) (h, d, a) = case c of
  "forward" -> (h + x, d + (a * x), a)
  "down"    -> (h, d, a + x)
  "up"      -> (h, d, a - x)

dive :: [Command] -> Position -> NextPos -> Position
dive []  p f    = p
dive [c] p f    = f c p 
dive (c:xs) p f = dive xs (f c p) f

toi :: [Char] -> Int
toi c = read c :: Int

parse :: [String] -> [Command]
parse []        = []
parse (x:y:xs)  = (x, toi y):(parse xs)

calc :: Position -> Int
calc (x,y,_) = x * y

main = do
    contents <- readFile "day_02.input"
    let xs = parse $ words contents
    putStrLn "solution 1"
    print $ calc $ dive xs (0, 0, 0) nextPos

    putStrLn "solution 2"
    print $ calc $ dive xs (0, 0, 0) nextPos'

