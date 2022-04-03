module Day3 where

import Data.List (transpose)
import Data.Function ((&))

type Digits = [Int]

parseLine :: String -> Digits
parseLine = map (\c -> read [c])

binaryToDecimal :: Digits -> Int
binaryToDecimal = foldl (\acc x -> acc * 2 + x) 0

zeroesOnesCount :: Digits -> (Int, Int)
zeroesOnesCount numbers = (zeroes, ones) where
  zeroes = length $ filter (== 0) numbers
  ones   = length $ filter (== 1) numbers

mostFreq :: [(Int, Int)] -> [Int]
mostFreq = map (\(zeroes, ones) -> if zeroes > ones then 0 else 1)

leastFreq :: [(Int, Int)] -> [Int]
leastFreq = map (\(zeroes, ones) -> if zeroes < ones then 0 else 1)

------------------------------------------------------------------------------------------------------------------------

data Frequencies =
  Frequencies {
    zeroes :: Int,
    ones :: Int
  } deriving (Show, Eq)

frequenciesMostCommon :: Frequencies -> Int -> Int
frequenciesMostCommon (Frequencies zeroes ones) ifEqual | zeroes == ones = ifEqual
                                                        | otherwise      = if zeroes > ones then 0 else 1

frequenciesLeastCommon :: Frequencies -> Int -> Int
frequenciesLeastCommon (Frequencies zeroes ones) ifEqual | zeroes == ones = ifEqual
                                                         | otherwise      = if zeroes < ones then 0 else 1

data FocusedList =
  FocusedList {
    prev :: [Int],
    current :: Int,
    remaining :: [Int]
  } deriving (Show, Eq)

focusedListCurrent :: FocusedList -> Int
focusedListCurrent (FocusedList _ current _) = current

filterWithCurrent :: Int -> [FocusedList] -> [FocusedList]
filterWithCurrent current = filter (\(FocusedList _ current' _) -> current == current')

focusedListsFrequences :: [FocusedList] -> Frequencies
focusedListsFrequences lists = Frequencies zeroes ones where
  zeroes = length $ filter (== 0) $ map focusedListCurrent lists
  ones   = length $ filter (== 1) $ map focusedListCurrent lists

restoreInitialList :: FocusedList -> Digits
restoreInitialList (FocusedList prev current remaining) = reverse prev ++ [current] ++ remaining

oxygenGeneratorRating :: [FocusedList] -> Digits
oxygenGeneratorRating [single] = restoreInitialList single
oxygenGeneratorRating lists = oxygenGeneratorRating advanced where
  frequencies = focusedListsFrequences lists
  mostCommon = frequenciesMostCommon frequencies 1
  filtered = filterWithCurrent mostCommon lists
  advanced = map advanceFocusedList filtered

co2ScrubberRating :: [FocusedList] -> Digits
co2ScrubberRating [single] = restoreInitialList single
co2ScrubberRating lists = co2ScrubberRating advanced where
  frequencies = focusedListsFrequences lists
  leastCommon = frequenciesLeastCommon frequencies 0
  filtered = filterWithCurrent leastCommon lists
  advanced = map advanceFocusedList filtered

advanceFocusedList :: FocusedList -> FocusedList
advanceFocusedList (FocusedList prev current (r : remaining)) = FocusedList (current : prev) r remaining
advanceFocusedList l = l

mkFocusedList :: [Int] -> FocusedList
mkFocusedList list = FocusedList [] (head list) (tail list)

------------------------------------------------------------------------------------------------------------------------

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

  --

  let bitLines = map mkFocusedList numbers
  let oxygenGeneratorRating' = oxygenGeneratorRating bitLines & binaryToDecimal
  let co2ScrubberRating' = co2ScrubberRating bitLines & binaryToDecimal
  let lifeSupportRating = oxygenGeneratorRating' * co2ScrubberRating'
  putStrLn $
    "Day 3. Part 2. Life support rating: " ++ show lifeSupportRating ++
    ", CO2 scrubber rating = " ++ show co2ScrubberRating' ++
    ", oxygen generator rating = " ++ show oxygenGeneratorRating'