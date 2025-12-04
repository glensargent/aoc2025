module Main where

import Data.Array

main :: IO ()
main = do
  input <- lines <$> readFile "inputs/day04.txt"
  let grid = makeGrid input
  print $ countAccessible grid

makeGrid :: [[a]] -> Array (Int, Int) a
makeGrid rows = listArray ((0, 0), (height - 1, width - 1)) (concat rows)
  where
    height = length rows
    width = length (head rows)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (r, c) = [(r+xr, c+xc) | xr <- [-1..1], xc <- [-1..1], (xr,xc) /= (0,0)]

countAround :: Array (Int, Int) Char -> (Int, Int) -> Int
countAround grid pos = length $ filter isRoll validNeighbors
  where
    validNeighbors = filter (inRange (bounds grid)) (neighbors pos)
    isRoll p = grid ! p == '@'

isAccessible :: Array (Int, Int) Char -> (Int, Int) -> Bool
isAccessible grid pos = grid ! pos == '@' && countAround grid pos < 4

countAccessible :: Array (Int, Int) Char -> Int
countAccessible grid = length $ filter (isAccessible grid) (indices grid)
