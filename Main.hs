module Main where

import qualified InputHelpers (readInts)
import qualified Day1 (part1, part2)

main :: IO ()
main = do
  nums <- InputHelpers.readInts "input/day1"
  print (Day1.part1 nums)
  print (Day1.part2 nums)