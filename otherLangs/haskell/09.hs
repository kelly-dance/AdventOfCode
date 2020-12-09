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

checkIndex :: [Int] -> Int -> Bool
checkIndex input i = (input !! (i + 25)) `notElem` map sum (combinations 2 $ take 25 $ drop i input)

part1 :: [Int] -> Int
part1 input = input !! (partial + 25)
  where
    partial = fromMaybe 0 $ find (checkIndex input) [0 .. length input - 26]

part2 :: [Int] -> Int -> [Int]
part2 input target = fromMaybe [] $ find (\xs -> sum xs == target) $ contiguousSubsequences input 2

contiguousSubsequences :: [Int] -> Int -> [[Int]]
contiguousSubsequences input min = concatMap  straights [min .. length input - 1]
  where
    straights len = map (\o -> take len $ drop o input)[0 .. length input - len]

combinations :: Int -> [Int] -> [[Int]]
combinations 1 ns = map (:[]) ns
combinations k ns = concat $ zipWith (\t i -> map (t:) (combinations (k - 1) (drop i ns))) ns [1 .. length ns]
