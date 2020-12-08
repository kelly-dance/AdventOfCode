import Control.Monad ()
import Data.List.Split ( splitOn )

main = do
  contents <- readFile "inputs/03.txt"
  let input = splitOn "\n" contents
      slopes = [(1,1),(1,3),(1,5),(1,7),(2,1)]
      solver = solveSlope input
  print (solver (1,3))
  print (product (map solver slopes))

solveSlope :: [[Char]]  -> (Int,Int) -> Int
solveSlope ls (dy,dx) = length (filter (\y -> '#' == ((ls !! y) !! ((y * dx `div` dy) `mod` length (ls !! y)))) [dy, dy * 2 .. length ls - 1])

