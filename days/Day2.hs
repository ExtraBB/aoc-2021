module Day2(part1, part2) where

move :: [String] -> [Int] -> [Int]
move ("forward" :x:_)  (pos:depth:_) = [pos + read x, depth]
move ("up"      :x:_)  (pos:depth:_) = [pos, depth - read x]
move ("down"    :x:_)  (pos:depth:_) = [pos, depth + read x]
move _ x                             = x

part1 :: [String] -> Int
part1 lines = solution $ foldr (move . words) [0,0] lines

move2 :: [String] -> [Int] -> [Int]
move2 ("forward" :x:_)  (pos:depth:aim:_) = [pos + read x, depth + (aim * read x), aim]
move2 ("up"      :x:_)  (pos:depth:aim:_) = [pos, depth, aim - read x]
move2 ("down"    :x:_)  (pos:depth:aim:_) = [pos, depth, aim + read x]
move2 _                  x                = x

part2 :: [String] -> Int 
part2 lines = solution $ foldr (move2 . words) [0,0,0] (reverse lines)

solution :: [Int] -> Int
solution (x:y:xs) = x * y
solution _ = 0