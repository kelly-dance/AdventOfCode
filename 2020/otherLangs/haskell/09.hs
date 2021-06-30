import Control.Monad ()
import Data.List ( find )
import Data.Maybe ( fromMaybe )

main :: IO ()
main = do
  contents <- readFile "inputs/09.txt"
  let input = readInts $ lines contents
      p1 = part1 input
      p2 = part2 input p1
  print p1
  print $ minimum p2 + maximum p2

readInts :: [String] -> [Int]
readInts = map read

part1 :: [Int] -> Int
part1 input = check (take 25 input) (drop 25 input)
  where
    check _ [] = 0
    check prior rest = if any (\xs -> sum xs == head rest) (combinations 2 prior) then
      check (tail prior ++ [head rest]) $ tail rest
    else
      head rest

part2 :: [Int] -> Int -> [Int]
part2 input target = fromMaybe [] $ find (\xs -> sum xs == target) $ contiguousSubsequences input 2

contiguousSubsequences :: [Int] -> Int -> [[Int]]
contiguousSubsequences input min = concatMap  straights [min .. length input - 1]
  where
    straights len = map (\o -> take len $ drop o input)[0 .. length input - len]

combinations :: Int -> [Int] -> [[Int]]
combinations 1 ns = map (:[]) ns
combinations k ns = concat $ zipWith (\t i -> map (t:) (combinations (k - 1) (drop i ns))) ns [1 .. length ns]
