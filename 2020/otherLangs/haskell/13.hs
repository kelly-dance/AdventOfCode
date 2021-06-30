import Control.Monad ()
import Data.List.Split ( splitOn )

main :: IO ()
main = do
  contents <- readFile "inputs/13.txt"
  let ls = lines contents
      goal = read $ head ls :: Integer
      rawBusses = map (\s -> if s == "x" then 0 else read s) $ splitOn "," $ last ls :: [Integer]
      withIdx = filter (\(b, _) -> b /= 0) $ zip rawBusses [0..]
      filteredBusses = filter (/=0) rawBusses
  print $ part1 goal filteredBusses
  print $ part2 withIdx

part1 :: Integer -> [Integer] -> Integer
part1 goal input = best * gap best
  where
    best = foldl1 compare input
    gap n = n - goal `mod` n
    compare a b = if gap a < gap b then a else b

part2 :: [(Integer, Integer)] -> Integer
part2 input = crt $ map (\(n, i) -> ((n - i) `mod` n, n)) input

-- https://stackoverflow.com/questions/35529211/chinese-remainder-theorem-haskell
crt :: (Integral a, Foldable t) => t (a, a) -> a
crt nums = fst $ foldr go (0, 1) nums
    where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
        where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    a `inv` m = let (_, i, _) = gcd a m in i `mod` m

    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
        where (g, s, t) = gcd (b `mod` a) a