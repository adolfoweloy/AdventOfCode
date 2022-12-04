import qualified Lib.Util as U
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe (isJust)

type Line = [Int]
type Board = [Line]
type Bingo = [Board]
type Numbers = [Int]


bingo :: [String] -> Bingo
bingo xs = map (\ys' -> (parseInts ' ') <$> ys') ys
  where 
    ys = U.splitOn "" zebra
    zebra = tail $ tail xs


parseInts :: Char -> String -> Numbers
parseInts sep xs = map (\x -> read x :: Int) xs'
  where xs' = U.splitOn sep xs


lineMatches :: Numbers -> Line -> Bool
lineMatches ns line = length (S.intersection (S.fromList ns) (S.fromList line)) >= 5


isWinner :: Numbers -> Board -> Bool
isWinner ns board = isJust $ L.find id result
  where 
    result = ((lineMatches ns) <$> board) ++ ((lineMatches ns) <$> (L.transpose board))


findWinner :: Numbers -> (Numbers, Bingo) -> (Numbers, Board)
findWinner _ ([], _) = error "no winner"
findWinner tmp (n:ns, boards) = case L.find (isWinner (n:tmp)) boards of
  Just winner -> (n:tmp, winner)
  _ -> findWinner (n:tmp) (ns, boards)



findLastWinner :: Numbers -> (Numbers, Bingo) -> [(Int, Numbers, Board)]
findLastWinner _ ([], _) = []
findLastWinner tmp (n:ns, boards) = case L.find (isWinner (n:tmp)) boards of
  Just winner -> (n, n:tmp, winner):(findLastWinner (n:tmp) (ns, boards))
  Nothing     -> findLastWinner (n:tmp) (ns, boards)


calc :: Int -> Numbers -> Board -> Int
calc lastNum ns board = s * lastNum
  where 
    a  = S.fromList ns
    b  = S.fromList (U.flatMap board)
    df = S.difference b a
    s  = sum df


solve :: [String] -> Int
solve xs = calc lastNum ns winner
  where
    ds = parseInts ',' (head xs)
    bs = bingo xs
    (ns,winner) = findWinner [] (ds, bs)
    lastNum = head ns


solve2 :: [String] -> Int
solve2 xs = calc lastNum numbers winner
  where
    ds = parseInts ',' (head xs)
    bs = bingo xs
    (lastNum, numbers, winner) = last $ findLastWinner [] (ds, bs)



main = do
    content <- readFile "day_04.small.input"
    print $ solve (lines content)
    print $ solve2 (lines content)

