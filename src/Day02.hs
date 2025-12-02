module Main where

import Data.List.Split

main :: IO ()
main = do
  input <- splitOn "," <$> readFile "inputs/day02.txt"
  let ranges = concatMap extractRange input
  print $ sum (map read (filter checkNumber ranges) :: [Int])
  print $ sum (map read (filter checkNumber2 ranges) :: [Int])

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

checkNumber2 :: String -> Bool
checkNumber2 input =
  any isRepeated [1 .. length input `div` 2]
  where
    isRepeated len = length input `mod` len == 0 && all (== prefix) chunks
      where
        prefix = take len input
        chunks = chunksOf len input

