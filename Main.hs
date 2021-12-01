module Main where

import qualified MyLib (readLines, readInts)

main :: IO ()
main = do
  nums <- MyLib.readInts "input/day1"
  putStrLn (show (day1_1 nums (0, maxBound)))

day1_1 :: [Int] -> (Int, Int) -> Int
day1_1 [] (total, _) = total
day1_1 (x:xs) (total, last) = day1_1 xs (if x > last then (total + 1, x) else (total, x))
