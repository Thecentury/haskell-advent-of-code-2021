{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}

module Day2 where

import Data.List (stripPrefix)

data Command =
  Down Int
  | Up Int
  | Forward Int
  deriving (Show)

parseCommand :: String -> Command
parseCommand (stripPrefix "forward " -> Just number) = Forward (read number)
parseCommand (stripPrefix "up " -> Just number) = Up (read number)
parseCommand (stripPrefix "down " -> Just number) = Down (read number)
parseCommand other = error $ "Invalid command '" ++ other ++ "'"

data Position =
  Position {
    x :: Int,
    y :: Int,
    aim :: Int
  } deriving (Show)

applyCommand :: Position -> Command -> Position
applyCommand (Position x y aim) (Down n) = Position x y (aim + n)
applyCommand (Position x y aim) (Up n) = Position x y (aim - n)
applyCommand (Position x y aim) (Forward n) = Position (x + n) (y + aim * n) aim

applyCommands :: Position -> [Command] -> Position
applyCommands pos = foldl applyCommand pos

run :: IO ()
run = do
  content <- readFile "input/day2.txt"
  let commands = map parseCommand $ lines content
  let initialPosition = Position 0 0 0
  let finalPosition = foldl applyCommand initialPosition commands
  putStrLn $ "Final position: " ++ show finalPosition

  let positionsProduct = finalPosition.x * finalPosition.y
  putStrLn $ "Product: " ++ show positionsProduct
