import Control.Monad ()
import Data.List ( sort ) 

main :: IO ()
main = do
  contents <- readFile "inputs/10.txt"
  let input = map read $ lines contents :: [Int]
      sorted = sort input
      final = 0 : sorted ++ [last sorted + 3]
  print $ part1 final
  print $ part2 final


part1 :: [Int] -> Int
part1 input = uncurry (*) final
  where
    final = recurse (0,0) input
    recurse acc [_] = acc
    recurse (ones, threes) (x:xs) = 
      if head xs - x == 1
        then recurse (ones + 1, threes) xs
        else recurse (ones, threes + 1) xs

part2 :: [Int] -> Int
part2 input = memoized 0
  where from index
          | index == length input - 1 = 1
          | otherwise = out (input !! index) (index + 1) 0
        out source current acc
          | current >= length input|| input !! current - 3 > source = acc
          | otherwise = out source (current + 1) (acc + memoized current)
        memoized = (map from [0 ..] !!)

