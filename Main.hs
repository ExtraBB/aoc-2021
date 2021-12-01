module Main where

import qualified InputHelpers (readLines, readInts)

main :: IO ()
main = do
  nums <- InputHelpers.readInts "input/day1"
  print (day1 nums)
  print (day1part2 nums)

day1 :: [Int] -> Int
day1 (x:y:xs) = fromEnum (y > x) + day1 (y:xs)
day1 _ = 0

day1part2 :: [Int] -> Int
day1part2 xs@(u:v:w:x:y:z:_) = fromEnum ((x + y + z) > (u + v + w)) + day1part2 (tail xs)
day1part2 _ = 0
