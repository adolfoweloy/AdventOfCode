import Data.List
import System.IO
import Lib.Util

data Box     = Box Char deriving (Show)
data Command = C { count :: Int, from :: Int, to :: Int }

type Stack = [Maybe Box]
type Stacks = [Stack]
type Commands = [Command]


parse :: [String] -> (Stacks, Commands)
parse xs = undefined


-- once I have everything parsed, how do I move elements from
-- one list/stack to the other 
solution1 :: Stacks -> Commands -> [Char]
solution1 xs ys = undefined

solution2 :: Stacks -> Commands -> [Char]
solution2 xs ys = undefined


-- this is enough to create the list of stacks I wanted
matrix = transpose 

parseBoxes :: [String] -> Stacks
parseBoxes []     = []
parseBoxes (x:xs) = (boxes x) : parseBoxes xs

-- process one raw string into a typed list of boxes
boxes :: String -> Stack
boxes [] = []
boxes xs = b:(boxes rest) 
  where n    = next xs -- (Box, String)
        b    = fst n
        rest = snd n

-- fetches next box from a String input and saves the rest
next :: String -> (Maybe Box, String)
next xs = (box y, rest)
  where x    = splitAt 3 xs
        y    = fst x 
        rest = drop 1 $ snd x

-- tries to get a box from a three letters string
box :: String -> Maybe Box
box "   "   = Nothing
box (_:x:_) = Just (Box x)
box _       = Nothing


-- just using composition 
f _ = 1
g _ = 1


main = do
    contents <- readFile "day05.input.sample"
    let xs = (lines contents)

    putStrLn "solution 1"
    print (f xs)

    putStrLn "solution 2"
    print (g xs)
