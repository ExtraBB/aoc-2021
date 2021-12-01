module Main where

import qualified InputHelpers (readLines, readInts)

main :: IO ()
main = do
  nums <- InputHelpers.readInts "input/day1"
  print (part1 nums)
  print (part2 nums)

part1 :: [Int] -> Int
part1 xs@(x:y:_) = fromEnum (y > x) + part1 (tail xs)
part1 _ = 0

part2 :: [Int] -> Int
part2 = part1 . windows

windows :: [Int] -> [Int]
windows xs@(x:y:z:_) = (x + y + z) : windows (tail xs)
windows _ = []