import Control.Monad ()
import Data.List.Split ( splitOn )
import qualified Data.Set as Set

type Instruction = ([Char], Int)

main :: IO ()
main = do
  contents <- readFile "inputs/08.txt"
  let input = map parseLine $ lines contents
  print $ snd $ run input
  print $ snd $ head $ filter fst $ map run $ makeInstructions input

run :: [Instruction] -> (Bool, Int)
run instructions = executor instructions 0 0 Set.empty
  where
    executor :: [Instruction] -> Int -> Int -> Set.Set Int -> (Bool, Int)
    executor tape acc position seen
      | position `elem` seen = (False, acc)
      | position >= length tape = (True, acc)
      | otherwise
      = case tape !! position of
          ("acc", arg)
            -> executor tape (acc + arg) (position + 1) $ Set.insert position seen
          ("jmp", arg)
            -> executor tape acc (position + arg) $ Set.insert position seen
          ("nop", _)
            -> executor tape acc (position + 1) $ Set.insert position seen
    
makeInstructions :: [Instruction] -> [[Instruction]]
makeInstructions [] = []
makeInstructions (x:xs) = (toggleInstruction x : xs) : map (x:) (makeInstructions xs)

toggleInstruction :: Instruction -> Instruction
toggleInstruction ("jmp", arg) = ("nop", arg)
toggleInstruction ("nop", arg) = ("jmp", arg)
toggleInstruction i = i

parseLine ::[Char] -> Instruction
parseLine line = (head split, read $ filter (/='+') $ split !! 1)
  where
    split = splitOn " " line
