import Control.Monad ()
import Data.List.Split ( splitOn )
import Data.List ( find )

main :: IO ()
main = do
  contents <- readFile "inputs/05.txt"
  let input = map parseLine $ splitOn "\n" contents
  print $ part1 input
  print $ part2 input

part1 :: [Int] -> Int
part1 = foldl max 0

part2 :: [Int] -> Int
part2 input = case find (\x -> (x + 1) `notElem` input) input of
  Just n -> n + 1
  Nothing -> 0

parseLine :: [Char] -> Int
parseLine line = foldl reducer 0 $ map bin line
  where
    reducer :: Int -> Int -> Int
    reducer acc current = acc * 2 + current
    bin :: Char -> Int
    bin c = if c == 'B' || c == 'R' then 1 else 0
