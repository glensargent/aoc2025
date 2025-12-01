module Main where

main :: IO ()
main = do
  input <- readFile "inputs/day01.txt"
  let (p1, p2) = handleTurns (map getMotion (lines input)) 50 (0, 0)
  print p1
  print p2

type Turn = (Char, Int)

getMotion :: String -> Turn
getMotion [] = (' ', 0)
getMotion (x : xs) = (x, read xs)

handleTurns :: [Turn] -> Int -> (Int, Int) -> (Int, Int)
handleTurns [] _ acc = acc
handleTurns (turn : rest) currentNum (landings, total) =
  handleTurns rest currentResult (landings', total + passes)
  where
    (passes, currentResult) = getTurnResult turn currentNum
    landings' = if currentResult == 0 then landings + 1 else landings

getTurnResult :: Turn -> Int -> (Int, Int)
getTurnResult ('R', x) currentVal = divMod (currentVal + x) 100
getTurnResult ('L', x) currentVal = (passes, newPos)
  where
    newPos = (currentVal - x) `mod` 100
    passes
      | currentVal == 0 = x `div` 100
      | x >= currentVal = 1 + (x - currentVal) `div` 100
      | otherwise = 0
getTurnResult _ currentVal = (0, currentVal)
