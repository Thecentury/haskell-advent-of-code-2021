module Day4 where

import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Read (readMaybe)

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]] where
  f c l@([]:xs) | c == delimiter = l
                | otherwise = (c:[]):xs
  f c l@(x:xs)  | c == delimiter = []:l
                | otherwise = (c:x):xs
  f _ [] = []

parseToInt :: String -> Int
parseToInt s =
  case readMaybe s of
    Just i -> i
    Nothing -> error $ "Could not parse to int '" ++ s ++ "'"

--------------------------------------------------------------------------------

data Cell =
  Unmarked Int | Marked Int
  deriving (Eq, Show)

isMarked :: Cell -> Bool
isMarked (Marked _) = True
isMarked _ = False

markCell :: Int -> Cell -> Cell
markCell n (Unmarked cell) | n == cell = Marked n
markCell _ cell = cell

type View = [[Cell]]

markNumberInView :: Int -> View -> View
markNumberInView n = map (map (markCell n))

isViewComplete :: View -> Bool
isViewComplete = any (all isMarked)

allUnmarkedOfView :: View -> [Int]
allUnmarkedOfView =
  concatMap $ mapMaybe (\cell -> case cell of
                                    Unmarked n -> Just n
                                    _ -> Nothing)

data Board =
  Board {
    rows :: View,
    columns :: View
  } deriving (Eq, Show)

mkBoard :: [[Int]] -> Board
mkBoard numbers = Board rows columns where
  rows = map (map Unmarked) numbers
  columns = map (map Unmarked) $ transpose numbers

markNumber :: Int -> Board -> Board
markNumber number (Board rows columns) =
  Board (markNumberInView number rows) (markNumberInView number columns)

isBoardComplete :: Board -> Bool
isBoardComplete (Board rows columns) = isViewComplete rows || isViewComplete columns

findFirstCompleteBoard :: [Int] -> [Board] -> Maybe (Board, Int)
findFirstCompleteBoard [] _ = Nothing
findFirstCompleteBoard (number : numbers) boards = completeBoard where
  boards' = map (markNumber number) boards
  firstCompleteBoard = listToMaybe $ filter isBoardComplete boards'
  completeBoard =
    case firstCompleteBoard of
      Nothing -> findFirstCompleteBoard numbers boards'
      Just board -> Just (board, number)

allUnmarkedNumbers :: Board -> [Int]
allUnmarkedNumbers (Board rows _) =
  allUnmarkedOfView rows

--------------------------------------------------------------------------------

runDay4 :: [String] -> Int
runDay4 lines' =
  case findFirstCompleteBoard randomNumbers boards of
    Nothing -> error "Could not find a complete board"
    Just (completeBoard, finalNumber) -> finalScore where
      unmarkedNumbers = allUnmarkedNumbers completeBoard
      finalScore = finalNumber * (sum unmarkedNumbers)
  where
   randomNumbers = map (\x -> read x :: Int) $ splitBy ',' (head lines')
   boardLines = map (map ((map parseToInt) . filter (not . null) . (splitBy ' '))) $ map tail $ chunksOf 6 $ tail lines'
   boards = map mkBoard boardLines

run :: IO ()
run = do
  content <- readFile "input/day4.txt"
  let lines' = lines content
  let finalScore = runDay4 lines'
  putStrLn $ "Day 4. Final score: " ++ show finalScore

  return ()