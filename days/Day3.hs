module Day3 (part1, part2) where
import Data.Char (digitToInt)
import Control.Monad.State
import Data.Bits (complement)

position :: Int -> String -> Int
position n word = digitToInt (word !! n)

mostCommonBitAt :: [String] -> Int -> Int
mostCommonBitAt lines n = fromEnum $ count >= length lines - count
    where count = sum (map (position n) lines)

checkBitAt :: Int -> Int -> String -> Bool 
checkBitAt pos bit x = position pos x == bit

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

fromBinaryString :: String -> Int 
fromBinaryString s = fromBinary $ map digitToInt s

mostCommonBits :: [String] -> [Int]
mostCommonBits lines = map (mostCommonBitAt lines) [0..length (head lines) - 1]

part1 :: [String] -> Int
part1 lines = fromBinary bin * fromBinary (map (1 -) bin)
    where bin = mostCommonBits lines

keepWithBitAt :: Int -> Int -> [String] -> [String]
keepWithBitAt pos bit = filter (checkBitAt pos bit)

keepWithMostCommonBitAt :: Int -> [String] -> [String]
keepWithMostCommonBitAt pos xs@(x:y:_) = keepWithBitAt pos (mostCommonBitAt xs pos) xs
keepWithMostCommonBitAt pos xs = xs

keepWithLeastCommonBitAt :: Int -> [String] -> [String]
keepWithLeastCommonBitAt pos xs@(x:y:_) = keepWithBitAt pos (1 - mostCommonBitAt xs pos) xs
keepWithLeastCommonBitAt pos xs = xs

foldMostCommon :: [String] -> String
foldMostCommon lines = head $ foldr keepWithMostCommonBitAt lines (reverse [0..length (head lines) - 1])

foldLeastCommon :: [String] -> String
foldLeastCommon lines = head $ foldr keepWithLeastCommonBitAt lines (reverse [0..length (head lines) - 1])

part2 :: [String] -> Int
part2 lines = fromBinary mostCommon * fromBinary leastCommon
    where mostCommon = map digitToInt $ foldMostCommon lines
          leastCommon = map digitToInt $ foldLeastCommon lines