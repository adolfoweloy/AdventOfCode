import Data.List
import System.IO
import Lib.Util

-- rock | paper | scissor
data Choice = A | B | C |X | Y | Z deriving (Show, Read, Eq)

-- replacing what you chose to match the strategy
-- A, X -> Rock  -> 1
-- B, Y -> Paper -> 2
-- C, Z -> Scissor -> 3

toChoice s  = read s :: Choice

mapChoice :: [String] -> (Choice, Choice)
mapChoice (x:y:[]) = (toChoice x, toChoice y)

mapWords     = (map words)
parseChoices = map mapChoice
parse        = parseChoices . mapWords

isRock    = (`elem` [A, X])
isPaper   = (`elem` [B, Y])
isScissor = (`elem` [C, Z])

-- functions created for the remap
lose :: Choice -> Choice
lose A = Z
lose B = X
lose C = Y

draw :: Choice -> Choice
draw A = X
draw B = Y
draw C = Z

win :: Choice -> Choice
win A = Y
win B = Z
win C = X

remap :: (Choice, Choice) -> (Choice, Choice)
remap (x, y) = case y of X -> (x, lose x)
                         Y -> (x, draw x)
                         Z -> (x, win x)
doRemap :: [(Choice, Choice)] -> [(Choice, Choice)]
doRemap xs = map remap xs

-- main logic starts here
score :: Choice -> Int
score x
  | isRock x    = 1
  | isPaper x   = 2
  | isScissor x = 3

result :: (Choice, Choice) -> Int 
result (A, Y) = 6
result (B, Z) = 6
result (C, X) = 6
result (x, y) = if (score x) == (score y) then 3 else 0

calc (x,y) = (score y) + (result (x,y))

mapResult :: [(Choice, Choice)] -> [Int]
mapResult xs = map calc xs

f = sum . mapResult . parse
g = sum . mapResult . doRemap . parse

main = do
    contents <- readFile "day02.input"
    let xs = (lines contents)

    putStrLn "solution 1"
    print $ (f xs)

    putStrLn "solution 2"
    print $ (g xs)
