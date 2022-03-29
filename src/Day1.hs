module Day1 where

import Data.Function ((&))

parse :: String -> Int
parse s = read s :: Int

numberOfDepthIncreases :: [String] -> Int
numberOfDepthIncreases strings = count where
  depths = fmap parse strings
  pairs = zip depths (tail depths)
  count = length $ filter (\(a, b) -> b > a) pairs

numberOfSlidingWindowIncreases :: [String] -> Int
numberOfSlidingWindowIncreases strings = count where
  depths = fmap parse strings
  triples = zip3 depths (drop 1 depths) (drop 2 depths) & map (\(a, b, c) -> a + b + c)
  pairsOfTriples = zip triples (tail triples)
  count = length $ filter (\(a, b) -> b > a) pairsOfTriples

run :: IO ()
run = do
  input <- readFile "input/day1.txt"
  let increases = numberOfSlidingWindowIncreases $ lines input
  print increases
