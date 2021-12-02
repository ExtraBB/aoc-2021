module Day2(part1, part2) where
import Control.Monad.State

data Direction = Forward Int | Down Int | Up Int

parseDirection :: String -> Direction
parseDirection input = case words input of
     ("forward" :x:_) -> Forward (read x)
     ("up"      :x:_) -> Up (read x)
     ("down"    :x:_) -> Down (read x)
     _ -> Forward 0

move :: [Direction] -> State (Int, Int) Int
move [] = do
    (pos, depth) <- get
    return (pos * depth)
move (x:xs) = do
    (pos, depth) <- get
    case x of
        Forward n   -> put (pos + n, depth)
        Up n        -> put (pos, depth - n)
        Down n      -> put (pos, depth + n)
    move xs

part1 :: [String] -> Int
part1 lines = (evalState . move) (map parseDirection lines) (0,0)

move2 :: [Direction] -> State (Int, Int, Int) Int
move2 [] = do
    (pos, depth, aim) <- get
    return (pos * depth)
move2 (x:xs) = do
    (pos, depth, aim) <- get
    case x of
        Forward n   -> put (pos + n, depth + aim * n, aim)
        Up n        -> put (pos, depth, aim - n)
        Down n      -> put (pos, depth, aim + n)
    move2 xs

part2 :: [String] -> Int 
part2 lines = (evalState . move2) (map parseDirection lines) (0,0,0)