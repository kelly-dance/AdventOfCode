import Control.Monad ()
import Data.List.Split ( splitOn )
import qualified Data.Set as Set

main :: IO ()
main = do
  contents <- readFile "inputs/06.txt"
  let input = map (map Set.fromList . lines) $ splitOn "\n\n" contents
  print $ sum $ map (Set.size . Set.unions) input
  print $ sum $ map (\(x:xs) -> Set.size $ foldl Set.intersection x xs) input