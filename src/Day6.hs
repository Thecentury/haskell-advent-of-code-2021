module Day6 where

import Text.Read (readMaybe)

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

advanceFish :: Age -> [Age]
advanceFish 0 = [6, 8]
advanceFish age = [age - 1]

runADay :: [Age] -> [Age]
runADay = concatMap advanceFish

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f = foldr (.) id (replicate n f)

runNDays :: Int -> [Age] -> [Age]
runNDays n = applyNTimes n runADay

run :: IO ()
run = do
  content <- readFile "input/day6.txt"
  let initialAges = map readInt $ splitBy ',' content
  let fishAfter80Days = runNDays 80 initialAges
  let fishCountAfter80Days = length fishAfter80Days
  putStrLn $ "Day 6.1. Fish after 80 days: " ++ show fishCountAfter80Days
  let fishAfter256Days = runNDays 256 initialAges
  let fishCountAfter256Days = length fishAfter256Days
  putStrLn $ "Day 6.2. Fish after 256 days: " ++ show fishCountAfter256Days
