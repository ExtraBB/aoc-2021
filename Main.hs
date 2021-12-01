module Main where

import qualified MyLib (readLines, readInts)

main :: IO ()
main = do
  nums <- MyLib.readInts "input/day1"
  print (day1 nums)

day1 :: [Int] -> Int
day1 [] = 0
day1 [_] = 0
day1 (x:y:xs) = fromEnum (y > x) + day1 (y:xs)
