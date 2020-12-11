import Control.Monad ()
import Data.Bifunctor ( Bifunctor(bimap) ) 

directions :: [(Int, Int)]
directions = [(1,1), (1,0), (0,1), (-1,-1), (-1,0), (0,-1), (1,-1), (-1,1)]

inbounds :: [[Char]] -> (Int, Int) -> Bool
inbounds state (x, y) = x >= 0 && y >= 0 && x < length state && y < length (head state)

get :: [[Char]] -> (Int, Int) -> Bool
get state (x, y) = inbounds state (x, y) && state !! x !! y == '#'

part1neighbors :: [[Char]] -> (Int, Int) -> Int
part1neighbors state (x, y) = length $ filter (get state) $ map (Data.Bifunctor.bimap (+x) (+y)) directions

part2neighbors :: [[Char]] -> (Int, Int) -> Int
part2neighbors state (x, y) = length $ filter (process 1) directions
  where
    process dist (dx, dy)
      | not $ inbounds state (r, c) = False
      | state !! r !! c == 'L' = False
      | state !! r !! c == '#' = True
      | otherwise = process (dist + 1) (dx, dy)
      where
        r = x + dx * dist
        c = y + dy * dist

solve :: [[Char]] -> ([[Char]] -> (Int, Int) -> Int) -> Int -> Int
solve state neighborFn deathReq = sum $ map (length . filter (== '#')) $ final state
  where
    final frame = if frame == next then frame else final next
      where next = iterate frame
    iterate frame = map (\x -> map (nextChar frame x) [0 .. length (head frame) - 1]) [0 .. length frame - 1]
    nextChar frame x y
      | self == '#' && neighbors >= deathReq = 'L'
      | self == 'L' && neighbors == 0 = '#'
      | otherwise = self
      where
          self = frame !! x !! y
          neighbors = neighborFn frame (x, y)

main :: IO ()
main = do
  contents <- readFile "inputs/11.txt"
  let input = lines contents
  print $ solve input part1neighbors 4
  print $ solve input part2neighbors 5
