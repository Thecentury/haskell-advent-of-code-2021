{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}
{- HLINT ignore "This binding for" -}
{- HLINT ignore "[-Wname-shadowing]" -}

module Day5 where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.Char (isDigit)
import Data.List (group, sort)

data Point =
  Point {
    x :: Int,
    y :: Int
  } deriving (Show, Eq, Ord)

data Line2d =
  Line2d {
    start :: Point,
    end :: Point
  } deriving (Show, Eq)

data ClassifiedLine =
  Horizontal { x1 :: Int, x2 :: Int, y :: Int }
  | Vertical { y1 :: Int, y2 :: Int, x :: Int }
  | Diagonal Line2d
  deriving (Show, Eq)

classifyLine :: Line2d -> ClassifiedLine
classifyLine l@(Line2d (Point x1 y1) (Point x2 y2))
  | y1 == y2 = Horizontal (min x1 x2) (max x1 x2) y1
  | x1 == x2 = Vertical (min y1 y2) (max y1 y2) x1
  | otherwise = Diagonal l

coveredPointsNoDiagonal :: ClassifiedLine -> [Point]
coveredPointsNoDiagonal (Horizontal x1 x2 y) = [Point x y | x <- [x1..x2]]
coveredPointsNoDiagonal (Vertical y1 y2 x) = [Point x y | y <- [y1..y2]]
coveredPointsNoDiagonal (Diagonal _) = []

coveredPoints :: ClassifiedLine -> [Point]
coveredPoints (Horizontal x1 x2 y) = [Point x y | x <- [x1..x2]]
coveredPoints (Vertical y1 y2 x) = [Point x y | y <- [y1..y2]]
coveredPoints (Diagonal (Line2d (Point x1 y1) (Point x2 y2))) =
  let
    len = abs (x2 - x1)
    dx = if x1 < x2 then 1 else -1
    dy = if y1 < y2 then 1 else -1
  in
    [Point x y | d <- [0..len], let x = x1 + d * dx, let y = y1 + d * dy]

--------------------------------------------------------------------------------

-- 365,809 -> 365,271

integerParser :: Parser Int
integerParser = read <$> many1 (satisfy isDigit)

pointParser :: Parser Point
pointParser = do
  x <- integerParser
  void $ char ','
  Point x <$> integerParser

lineParser :: Parser Line2d
lineParser = do
  start <- pointParser
  void $ string " -> "
  Line2d start <$> pointParser

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

--------------------------------------------------------------------------------

atLeast2LinesOverlapNoDiagonal :: [Line2d] -> Int
atLeast2LinesOverlapNoDiagonal inputLines = length $ filter (\g -> length g >= 2) covered where
  classified = map classifyLine inputLines
  covered = group $ sort $ concatMap coveredPointsNoDiagonal classified

atLeast2LinesOverlap :: [Line2d] -> Int
atLeast2LinesOverlap inputLines = length $ filter (\g -> length g >= 2) covered where
  classified = map classifyLine inputLines
  covered = group $ sort $ concatMap coveredPoints classified

--------------------------------------------------------------------------------

run :: IO ()
run = do
  content <- readFile "input/day5.txt"
  let lines' = mapM (regularParse lineParser) (lines content)
  case lines' of
    Right lines'' ->
      let
        countNoDiagonal = atLeast2LinesOverlapNoDiagonal lines''
        countWithDiagonal = atLeast2LinesOverlap lines''
      in
        do
          putStrLn $ "There are " ++ show countNoDiagonal ++ " lines that overlap at least 2 times (excluding diagonals)"
          putStrLn $ "There are " ++ show countWithDiagonal ++ " lines that overlap at least 2 times"
    Left err -> print err
  return ()