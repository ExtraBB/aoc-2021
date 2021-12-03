module Day3 (part1, part2) where
import Data.Char (digitToInt)
import Control.Monad.State
import Data.Bits (complement)

position :: Int -> String -> Int
position n word = digitToInt (word !! n)

countBitsAt :: [String] -> Int -> Int
countBitsAt lines n = if sum (map (position n) lines) > length lines `div` 2 then 1 else 0

fromBinaryS :: [Int] -> State (Int, Int) Int
fromBinaryS [] = do
    (total, n) <- get
    return total
fromBinaryS (x:xs) = do
    (total, n) <- get
    case x of
        1 -> put (total + 2^n, n + 1)
        _ -> put (total, n + 1)
    fromBinaryS xs

fromBinary :: [Int] -> Int
fromBinary xs = evalState (fromBinaryS . reverse $ xs) (0,0)

part1 :: [String] -> Int
part1 lines = fromBinary bin * fromBinary (map (1 -) bin)
    where bin = map (countBitsAt lines) [0..length (head lines) - 1]



part2 :: [String] -> Int
part2 = part1