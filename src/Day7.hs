module Day7 where

import Text.Read (readMaybe)
import Data.List (minimumBy)
import Data.Function (on, (&))
import Debug.Trace (traceShow)

--------------------------------------------------------------------------------

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]] where
  f c l@([]:xs) | c == delimiter = l
                | otherwise = [c] : xs
  f c l@(x:xs)  | c == delimiter = []:l
                | otherwise = (c:x):xs
  f _ [] = []

readInt :: String -> Int
readInt s =
  case readMaybe s of
    Just i -> i
    Nothing -> error $ "Could not parse to int '" ++ s ++ "'"

--------------------------------------------------------------------------------

totalMovementCost :: Int -> [Int] -> Int
totalMovementCost coord positions = sum $ map (\x -> abs (x - coord)) positions

cheapestPosition :: [Int] -> Int
cheapestPosition positions =
  let
    minPosition = minimum positions
    maxPosition = maximum positions
    allPositions = [minPosition..maxPosition]
  in
    allPositions
      & map (\x -> (x, totalMovementCost x positions))
      & minimumBy (compare `on` snd)
      & fst

run :: IO ()
run = do
  content <- readFile "input/day7.txt"
  let crabPositions = map readInt $ splitBy ',' content
  let minPosition = cheapestPosition crabPositions
  let fuelSpent = totalMovementCost minPosition crabPositions
  putStrLn $ "The cheapest position is " ++ show minPosition
  putStrLn $ "Part 1. Fuel spent: " ++ show fuelSpent
