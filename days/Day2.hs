module Day2(part1, part2) where
import Control.Monad.State

data Direction = Forward Int | Down Int | Up Int
data Rules = Part1 | Part2

parseDirection :: String -> Direction
parseDirection input = case words input of
     ("forward" :x:_) -> Forward (read x)
     ("up"      :x:_) -> Up (read x)
     ("down"    :x:_) -> Down (read x)
     _ -> Forward 0

part1 :: [String] -> Int
part1 lines = evalState (move Part1 lines) (0,0,0)

part2 :: [String] -> Int 
part2 lines = evalState (move Part2 lines) (0,0,0)

move :: Rules -> [String] -> State (Int, Int, Int) Int
move _ [] = do
    (pos, depth, aim) <- get
    return (pos * depth)
move rules (x:xs) = do
    (pos, depth, aim) <- get
    case direction of
        Forward steps   -> put (pos + steps, depth + ax * aim * steps, aim)
        Up      steps   -> put (pos, depth - steps * dx, aim - steps * ax)
        Down    steps   -> put (pos, depth + steps * dx, aim + steps * ax)
    move rules xs
        where  
            direction = parseDirection x
            (dx, ax) = case rules of { Part1 -> (1,0);  Part2 -> (0,1)}