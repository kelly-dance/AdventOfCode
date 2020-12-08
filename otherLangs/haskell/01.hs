import Control.Monad ()
import Data.List.Split ( splitOn )

main = do
  contents <- readFile "inputs/01.txt"
  let input = readInts (splitOn "\n" contents)
      part1 = head (filter ((2020==) . sum) $ combinations 2 input)
      part2 = head (filter ((2020==) . sum) $ combinations 3 input)
  print (product part1)
  print (product part2)

readInts :: [String] -> [Int]
readInts = map read

combinations :: Int -> [Int] -> [[Int]]
combinations 1 ns = map (:[]) ns
combinations k ns = concat (zipWith
       (\t i -> map (t:) (combinations (k - 1) (drop i ns)))
    ns [1..])

