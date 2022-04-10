module Day6 where

import Text.Read (readMaybe)
import Data.List (sort, group, groupBy, sortOn)
import Data.Function (on)
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

type Age = Int

data SameAgeFish = SameAgeFish {
  age :: Age,
  count :: Int
} deriving (Show, Eq)

groupByAge :: [Age] -> [SameAgeFish]
groupByAge = map (\xs -> SameAgeFish (head xs) (length xs)) . group . sort

advanceFish :: SameAgeFish -> [SameAgeFish]
advanceFish (SameAgeFish 0 count) = [SameAgeFish 6 count, SameAgeFish 8 count]
advanceFish (SameAgeFish age count) = [SameAgeFish (age - 1) count]

runADay :: [SameAgeFish] -> [SameAgeFish]
runADay = map collapseGroup . groupBy ((==) `on` age) . sortOn age . concatMap advanceFish where
  collapseGroup :: [SameAgeFish] -> SameAgeFish
  collapseGroup counts = SameAgeFish (age $ head counts) (sum $ map count counts)

runADayWithTracing :: [SameAgeFish] -> [SameAgeFish]
runADayWithTracing fish =
  let fish' = runADay fish
  in traceShow fish' fish'

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f = foldr (.) id (replicate n f)

runNDays :: Int -> [SameAgeFish] -> [SameAgeFish]
runNDays n = applyNTimes n runADay

totalFishCount :: [SameAgeFish] -> Int
totalFishCount = sum . map count

run :: IO ()
run = do
  content <- readFile "input/day6.txt"
  let initialAges = groupByAge $ map readInt $ splitBy ',' content
  let fishAfter80Days = runNDays 80 initialAges
  let fishCountAfter80Days = totalFishCount fishAfter80Days
  putStrLn $ "Day 6.1. Fish after 80 days: " ++ show fishCountAfter80Days
  let fishAfter256Days = runNDays 256 initialAges
  let fishCountAfter256Days = totalFishCount fishAfter256Days
  putStrLn $ "Day 6.2. Fish after 256 days: " ++ show fishCountAfter256Days
