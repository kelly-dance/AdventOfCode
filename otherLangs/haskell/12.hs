import Control.Monad ()

type Point = (Int, Int)
type DirLoc = (Point, Point) -- Direction & Location
type Instruction = (Char, Int)

main :: IO ()
main = do
  contents <- readFile "inputs/12.txt"
  let input = map (\l -> (head l, read $ tail l)) $ lines contents :: [Instruction]
  print input
  print $ part1 input
  print $ part2 input

addPoints :: Point -> Point -> Point
addPoints (x1,y1) (x2, y2) = (x1 + x2, y1 + y2)

scale :: Point -> Int -> Point
scale (x, y) magnitude = (x * magnitude, y * magnitude)

rotate :: Point -> Int -> Point
rotate point degrees = foldl (\p _ -> single p) point [1 .. ((degrees `div` 90) `mod` 4)]
  where single (x,y) = (y, -x)

part1 :: [Instruction] -> Int
part1 input = solve input p1apply (0, 1)

part2 :: [Instruction] -> Int
part2 input = solve input p2apply (1, 10)

solve :: [Instruction] -> (DirLoc -> Instruction -> DirLoc) -> Point -> Int
solve input applier start = abs (fst final) + abs (snd final)
  where 
    final :: Point
    final = snd $ foldl applier (start, (0, 0)) input

p1apply :: DirLoc -> Instruction -> DirLoc
p1apply (dir, loc) (ins, arg)
      | ins == 'L' = (rotate dir arg, loc)
      | ins == 'R' = (rotate dir (-arg), loc)
      | ins == 'N' = (dir, addPoints loc $ scale (1, 0) arg)
      | ins == 'E' = (dir, addPoints loc $ scale (0, 1) arg)
      | ins == 'S' = (dir, addPoints loc $ scale (-1, 0) arg)
      | ins == 'W' = (dir, addPoints loc $ scale (0, -1) arg)
      | ins == 'F' = (dir, addPoints loc $ scale dir arg)

p2apply :: DirLoc -> Instruction -> DirLoc
p2apply (waypt, loc) (ins, arg)
      | ins == 'L' = (rotate waypt arg, loc)
      | ins == 'R' = (rotate waypt (-arg), loc)
      | ins == 'N' = (addPoints waypt $ scale (1, 0) arg, loc)
      | ins == 'E' = (addPoints waypt $ scale (0, 1) arg, loc)
      | ins == 'S' = (addPoints waypt $ scale (-1, 0) arg, loc)
      | ins == 'W' = (addPoints waypt $ scale (0, -1) arg, loc)
      | ins == 'F' = (waypt, addPoints loc $ scale waypt arg)
