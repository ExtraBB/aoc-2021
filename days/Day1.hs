module Day1 (part1, part2) where

part1 :: [Int] -> Int
part1 xs@(x:y:_) = fromEnum (y > x) + part1 (tail xs)
part1 _ = 0

part2 :: [Int] -> Int
part2 = part1 . windows

windows :: [Int] -> [Int]
windows xs@(x:y:z:_) = (x + y + z) : windows (tail xs)
windows _ = []