module Utils where

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