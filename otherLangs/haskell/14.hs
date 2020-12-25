import Control.Monad ()
import Data.List.Split ( splitOn )
import Data.Ord ()

-- INCOMPLETE

main :: IO ()
main = do
  contents <- readFile "inputs/14.txt"
  let input = map undefined $ lines contents
  print input


parseLine line = helper (head halves) (last halves)
  where
    halves = splitOn " = " line
    helper "mask" right = undefined
    helper left right = read (filter (\c -> ord c >= 48 && ord c <= 57) left)
