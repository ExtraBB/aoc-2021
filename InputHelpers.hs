module InputHelpers (readLines, readInts) where

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

readInts :: FilePath -> IO [Int]
readInts path = do
    lines <- readLines path
    return (map read lines)