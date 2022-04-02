module Day3 where

import Data.List (transpose)

parseLine :: String -> [Int]
parseLine = map (\c -> read [c])

binaryToDecimal :: [Int] -> Int
binaryToDecimal = foldl (\acc x -> acc * 2 + x) 0

zeroesOnesCount :: [Int] -> (Int, Int)
zeroesOnesCount numbers = (zeroes, ones) where
  zeroes = length $ filter (== 0) numbers
  ones   = length $ filter (== 1) numbers

mostFreq :: [(Int, Int)] -> [Int]
mostFreq = map (\(zeroes, ones) -> if zeroes > ones then 0 else 1)

leastFreq :: [(Int, Int)] -> [Int]
leastFreq = map (\(zeroes, ones) -> if zeroes < ones then 0 else 1)

run :: IO ()
run = do
  content <- readFile "input/day3.txt"
  let numbers = map parseLine $ lines content
  let frequencies = map zeroesOnesCount $ transpose numbers

  let mostFrequent = mostFreq frequencies
  let leastFrequent = leastFreq frequencies

  let gamma = binaryToDecimal mostFrequent
  let epsilon = binaryToDecimal leastFrequent
  let consumption = gamma * epsilon
  putStrLn $ "Day 3. Consumption = " ++ show consumption ++ ", gamma = " ++ show gamma ++ ", epsilon = " ++ show epsilon