import Control.Monad ()
import Data.List.Split ( splitOn )
import qualified Data.Set as Set

main :: IO ()
main = do
  contents <- readFile "inputs/08.txt"
  let input = map parseLine $ lines contents
  print $ executor input 0 0 Set.empty

executor :: [([Char], Int)] -> Int -> Int -> Set.Set Int -> Int
executor tape acc position seen = if position `elem` seen then acc else case tape !! position of
  ("acc", arg) -> executor tape (acc + arg) (position + 1) (Set.union seen $ Set.singleton position)
  ("jmp", arg) -> executor tape acc (position + arg) (Set.union seen $ Set.singleton position)
  ("nop", _) -> executor tape acc (position + 1) (Set.union seen $ Set.singleton position)


parseLine ::[Char] -> ([Char], Int)
parseLine line = (head split, read $ filter (/='+') $ split !! 1)
  where
    split = splitOn " " line
