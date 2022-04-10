module Day7 where

import Text.Read (readMaybe)
import Data.List (minimumBy)
import Data.Function (on, (&))

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

type MovementCostFunction = Int -> Int -> Int

linearMovementCost :: Int -> Int -> Int
linearMovementCost a b = abs (a - b)

linearProgressionMovementCost :: Int -> Int -> Int
linearProgressionMovementCost a b =
  let
    n = abs (a - b)
  in
    ((n + 1) * n) `div` 2

totalMovementCost :: Int -> MovementCostFunction -> [Int] -> Int
totalMovementCost coord cost positions = sum $ map (cost coord) positions

cheapestPosition :: MovementCostFunction -> [Int] -> Int
cheapestPosition cost positions =
  let
    minPosition = minimum positions
    maxPosition = maximum positions
    allPositions = [minPosition..maxPosition]
  in
    allPositions
      & map (\x -> (x, totalMovementCost x cost positions))
      & minimumBy (compare `on` snd)
      & fst

run :: IO ()
run = do
  content <- readFile "input/day7.txt"
  let crabPositions = map readInt $ splitBy ',' content
  let cheapestPos = cheapestPosition linearMovementCost crabPositions
  let fuelSpent = totalMovementCost cheapestPos linearMovementCost crabPositions
  putStrLn $ "The cheapest position is (linear cost): " ++ show cheapestPos
  putStrLn $ "Part 1. Fuel spent: " ++ show fuelSpent

  let cheapestPos2 = cheapestPosition linearProgressionMovementCost crabPositions
  let fuelSpent2 = totalMovementCost cheapestPos2 linearProgressionMovementCost crabPositions
  putStrLn $ "The cheapest position is (progression cost): " ++ show cheapestPos2
  putStrLn $ "Part 2. Fuel spent: " ++ show fuelSpent2
