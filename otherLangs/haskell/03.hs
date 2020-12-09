import Control.Monad ()

main :: IO ()
main = do
  contents <- readFile "inputs/03.txt"
  let input = map (cycle . map (=='#')) $ lines contents
      slopes = [(1,1),(1,3),(1,5),(1,7),(2,1)]
  print $ solveSlope input (1,3)
  print $ product $ map (solveSlope input) slopes

solveSlope :: [[Bool]]  -> (Int,Int) -> Int
solveSlope ls (dy,dx) = length $ filter (\y -> ls !! y !! (y * dx `div` dy)) [dy, dy * 2 .. length ls - 1]
