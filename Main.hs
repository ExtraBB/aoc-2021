module Main where

import qualified InputHelpers (readInts)
import qualified Day1 (part1, part2)

main :: IO ()
main = do
  nums <- InputHelpers.readInts "input/day1"
  printDay 1 (Day1.part1 nums) (Day1.part2 nums)

printDay :: Show a => Int -> a -> a -> IO ()
printDay n part1 part2 = do
  print $ "=== Results for day " ++ show n ++ " ==="
  print $ "Part 1: " ++ show part1
  print $ "Part 2: " ++ show part2