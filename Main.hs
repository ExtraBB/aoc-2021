module Main where

import qualified InputHelpers (readLines, readInts)
import qualified Day1 (part1, part2)

main :: IO ()
main = day 1

day :: Int -> IO ()
day n = do
  input <- InputHelpers.readLines $ "input/day" ++ show n
  let (part1, part2) = runDay input n
  print $ "=== Results for day " ++ show n ++ " ==="
  print $ "Part 1: " ++ part1
  print $ "Part 2: " ++ part2

runDay :: [String] -> Int -> (String, String)
runDay lines n = case n of
  1 -> (show $ Day1.part1 nums, show $ Day1.part2 nums) where nums = map read lines
  _ -> ("None", "None")