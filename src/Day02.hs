module Main where

import Data.List.Split

main :: IO ()
main = do
  input <- splitOn "," <$> readFile "inputs/day02.txt"
  let ranges = concatMap extractRange input
  print $ sum (map read (filter checkNumber ranges) :: [Int])

extractRange :: String -> [String]
extractRange input =
  map show [start .. end]
  where
    [start, end] = map read (splitOn "-" input) :: [Int]

checkNumber :: String -> Bool
checkNumber input =
  left == right
  where
    (left, right) = splitAt (length input `div` 2) input
