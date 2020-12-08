import Control.Monad ()
import Data.List.Split ( splitOn )
import Data.List ( find )
import qualified Data.Set as Set

type SubBag = (Int, [Char])
type Bag = ([Char], [SubBag])

main :: IO ()
main = do
  contents <- readFile "inputs/07.txt"
  let input = map parseLine $ splitOn "\n" contents
  print $ part1 input
  print $ part2 input

part1 :: [Bag] -> Int
part1 input = Set.size $ recurse "shiny gold"
  where
    recurse :: [Char] -> Set.Set [Char]
    recurse root
      = foldl
        (\ s (c, _)
          -> Set.unions
            [s, Set.singleton c, recurse c])
        Set.empty
          $ filter
            (\ (_, contents) -> any (\ (_, key) -> key == root) contents) input

part2 :: [Bag] -> Int
part2 input = recurse "shiny gold"
  where
    recurse :: [Char] -> Int
    recurse target = case getBag target of
      Just (_, contents) -> sum $ map (\(amount, key) -> amount + amount * recurse key) contents
      -- This *should* never happen
      Nothing -> 0
    getBag :: [Char] -> Maybe Bag
    getBag target = find (\(key, _) -> key == target) input

parseLine :: [Char] -> Bag
parseLine line = (head halves, parseGroup backhalf)
  where
    halves :: [[Char]]
    halves = splitOn " bags contain " line
    backhalf :: [[[Char]]]
    backhalf = map (take 3 . splitOn " ") $ splitOn ", " (halves !! 1)
    parseBag :: [[Char]] -> SubBag
    parseBag [count, c1, c2] = (read count, c1 ++ " " ++ c2)
    parseGroup :: [[[Char]]] -> [SubBag]
    parseGroup [["no","other","bags."]] = []
    parseGroup group = map parseBag group
