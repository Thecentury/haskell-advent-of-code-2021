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
  | Other Line2d
  deriving (Show, Eq)

classifyLine :: Line2d -> ClassifiedLine
classifyLine l@(Line2d (Point x1 y1) (Point x2 y2))
  | y1 == y2 = Horizontal (min x1 x2) (max x1 x2) y1
  | x1 == x2 = Vertical (min y1 y2) (max y1 y2) x1
  | otherwise = Other l

coveredPoints :: ClassifiedLine -> [Point]
coveredPoints (Horizontal x1 x2 y) = [Point x y | x <- [x1..x2]]
coveredPoints (Vertical y1 y2 x) = [Point x y | y <- [y1..y2]]
coveredPoints (Other _) = []

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
    Right lines'' -> let count = atLeast2LinesOverlap lines'' in
      putStrLn $ "There are " ++ show count ++ " lines that overlap at least 2 times"
    Left err -> print err
  return ()