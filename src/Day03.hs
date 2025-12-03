module Main where

import Data.List.Split

main :: IO ()
main = do
  input <- lines <$> readFile "inputs/day03.txt"
  let xs = map (convertNums . chunksOf 1) input
  print $ sum $ map findBestPair xs

findBestPair :: [Int] -> Int
findBestPair [] = 0
findBestPair (x:xs) = snd $ foldl step (x, 0) xs
  where
    step (maxSoFar, best) n = (max maxSoFar n, max best (joinNums [maxSoFar, n]))

convertNums :: [String] -> [Int]
convertNums = map read

joinNums :: [Int] -> Int
joinNums xs = read (concatMap show xs)
