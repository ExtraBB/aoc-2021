module Day1 (part1, part2) where

part1 :: [Int] -> Int
part1 = increases 1

part2 :: [Int] -> Int
part2 = increases 3

increases :: Int -> [Int] -> Int
increases n xs = length $ filter id $ zipWith (<) xs (drop n xs)