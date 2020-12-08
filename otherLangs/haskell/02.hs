import Control.Monad ()
import Data.List.Split ( splitOn )

main = do
  contents <- readFile "inputs/02.txt"
  let input = map parseLine (splitOn "\n" contents)
      part1 = length (filter part1Checker input)
      part2 = length (filter part2Checker input)
  print part1
  print part2

readInts :: [String] -> [Int]
readInts = map read

part1Checker :: ([Int], Char, [Char]) -> Bool
part1Checker ([lower, upper], char, pass) = countChar char pass `elem` [lower..upper]

part2Checker :: ([Int], Char, [Char]) -> Bool
part2Checker ([lower, upper], char, pass) = (pass !! (lower - 1) == char) `xor` (pass !! (upper - 1) == char)

parseLine :: [Char] -> ([Int], Char, [Char])
parseLine line = do
  let atSpaces = splitOn " " line
      nums = readInts (splitOn "-" (head atSpaces))
      char = head (atSpaces !! 1)
  (nums, char, atSpaces !! 2)

countChar :: Char -> [Char] -> Int
countChar c str = helper str 0
  where
    helper "" count = count
    helper sub count =
      if head sub == c then
        helper (tail sub) (count + 1)
      else
        helper (tail sub) count

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)
