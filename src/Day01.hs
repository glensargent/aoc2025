module Main where

main :: IO ()
main = do
  input <- readFile "inputs/day01.txt"
  print $ part1 (lines input)
  putStrLn $ "Part 2: " ++ part2 input


part1 :: [String] -> Int
part1 inputs = handleTurns (map getMotion inputs) 50 0

type Turn = (Char, Int)
getMotion :: String -> Turn
getMotion [] = (' ', 0) -- obsolete, just getting rid of warning
getMotion (x : xs) = (x, read xs) 

handleTurns :: [Turn] -> Int -> Int -> Int
handleTurns [] _ acc = acc
handleTurns (turn:rest) currentNum acc
  | currentResult == 0 = handleTurns rest currentResult (acc + 1)
  | otherwise          = handleTurns rest currentResult acc
  where
    currentResult = getTurnResult turn currentNum

getTurnResult :: Turn -> Int -> Int
getTurnResult ('R', x) currentVal = mod (currentVal + x) 100
getTurnResult ('L', x) currentVal = mod (currentVal - x) 100
getTurnResult _ currentVal = currentVal

part2 :: String -> String
part2 _input = "TODO"
