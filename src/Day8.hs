module Day8 where

import qualified Data.List as List
import qualified Data.Map as Map
import Text.Parsec
import Text.ParserCombinators.Parsec
import Control.Monad (void)
import Text.Pretty.Simple (pPrint)

--------------------------------------------------------------------------------
-- unique signal patterns | four digit output value
-- dac abcf ac fdbcga dgcbae gcbfde fgcbd agfed adcgf cdbgfea | cbfa bcafdg cbfa bafcgd

type InputSignalWire = Char
type InputSignal = [InputSignalWire]
type Segment = Char
type OutputValue = [Segment]

data Entry = Entry {
  signalPatterns :: [InputSignal],
  outputValues :: [OutputValue]
} deriving (Show, Eq)

wordParser :: Parser String
wordParser = many1 letter <?> "word"

wordsParser :: Parser [String]
wordsParser =
  do
    w <- wordParser
    ws <- remainingWords
    return (w:ws)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingWords :: Parser [String]
remainingWords =
    Text.Parsec.try (space >> wordsParser)           -- Found comma?  More cells coming
    <|> return []                     -- No comma?  Return [], no more cells

recordParser :: Parser Entry
recordParser =
  do
    signalPatterns <- wordsParser
    void (string " | " <?> "delimiter")
    outputValues <- wordsParser
    eof
    return $ Entry (map List.sort signalPatterns) (map List.sort outputValues)

parseLine :: String -> Either ParseError Entry
parseLine = parse recordParser ""

--------------------------------------------------------------------------------

is1478 :: OutputValue -> Bool
is1478 s = go $ length s where
  go 2 = True -- 1
  go 4 = True -- 4
  go 3 = True -- 7
  go 7 = True -- 8
  go _ = False

countOf1478 :: [Entry] -> Int
countOf1478 = length . filter is1478 . concatMap outputValues

--------------------------------------------------------------------------------

data WireSegmentMapping = WireSegmentMapping {
  inputWire :: InputSignalWire,
  candidateSegments :: [Segment]
} deriving (Show, Eq)

data Guess =
  NoCandidates
  | OneCandidate (OutputValue, [WireSegmentMapping])
  | MultipleCandidates [(OutputValue, [WireSegmentMapping])]
  deriving (Show, Eq)

updateMultipleCandidates :: Guess -> Guess
updateMultipleCandidates (MultipleCandidates []) = NoCandidates
updateMultipleCandidates (MultipleCandidates [x]) = OneCandidate x
updateMultipleCandidates (MultipleCandidates xs) = MultipleCandidates xs
updateMultipleCandidates other = other

excludeImpossibleCandidates :: Guess -> WireSegmentMapping -> Guess
excludeImpossibleCandidates NoCandidates _ = NoCandidates
excludeImpossibleCandidates (OneCandidate (outputValue, mappings)) _ = OneCandidate (outputValue, mappings)
excludeImpossibleCandidates (MultipleCandidates candidates) (WireSegmentMapping wire segments) = updateMultipleCandidates $ MultipleCandidates $ map exclude candidates where
  exclude :: (OutputValue, [WireSegmentMapping]) -> (OutputValue, [WireSegmentMapping])
  exclude (outputValue, mappings) = (outputValue, excluded) where
    hasSegmentsForWire = any (\mapping -> inputWire mapping == wire) mappings
    excluded = if hasSegmentsForWire then
                map (\(WireSegmentMapping currentWire currentSegments) ->
                  if currentWire == wire then
                    WireSegmentMapping currentWire (segments `List.intersect` currentSegments)
                  else
                    WireSegmentMapping currentWire currentSegments) mappings
               else
                  mappings

excludeX :: [Guess] -> [Guess]
excludeX guesses = foldl go guesses guesses where
  go :: [Guess] -> Guess -> [Guess]
  go guesses' NoCandidates = guesses'
  go guesses' (OneCandidate (_, mappings)) = foldl (\gs mapping -> map (`excludeImpossibleCandidates` mapping) gs) guesses' mappings
  go guesses' (MultipleCandidates _) = guesses'

segmentsCountToPossibleSegments :: Map.Map Int [[Segment]]
segmentsCountToPossibleSegments = Map.fromList [
      (2, [['c', 'f']]), -- 1
      (3, [['a', 'c', 'f']]), -- 7
      (4, [['b', 'c', 'd', 'f']]), -- 4
      (5, [
        ['a', 'c', 'd', 'e', 'g'], -- 2
        ['a', 'c', 'd', 'f', 'g'], -- 3
        ['a', 'b', 'd', 'd', 'g']  -- 5
      ]),
      (6, [
        ['a', 'b', 'c', 'e', 'f', 'g'], -- 0
        ['a', 'b', 'd', 'e', 'f', 'g'], -- 6
        ['a', 'b', 'c', 'd', 'f', 'g']  -- 9
      ]),
      (7, [['a', 'b', 'c', 'd', 'e', 'f', 'g']]) -- 8
    ]

guessSegments :: InputSignal -> Guess
guessSegments signal =
  case Map.lookup (length signal) segmentsCountToPossibleSegments of
    Nothing -> NoCandidates
    Just [candidate] -> OneCandidate (candidate, map (`WireSegmentMapping` candidate) signal)
    Just multipleCandidates -> MultipleCandidates $ map (\candidate -> (candidate, map (`WireSegmentMapping` candidate) signal)) multipleCandidates

--loopGuesses :: [Guess] -> Maybe Guess
--loopGuesses guesses =
--  let
--    definiteGuesses = map head $ filter (\guess -> length guess == 1) guesses
--    knownWires = map candidateSegments definiteGuesses
--  in Nothing

--------------------------------------------------------------------------------

run :: IO ()
run = do
  let lineToParse = "dac abcf ac fdbcga dgcbae gcbfde fgcbd agfed adcgf cdbgfea | cbfa bcafdg cbfa bafcgd"
  case parseLine lineToParse of
    Left err -> print err
    Right entry ->
      do
        putStrLn $ "Input: " ++ show entry
        let guesses = map (\signal -> (signal, guessSegments signal)) $ signalPatterns entry
        putStrLn "Guesses:"
        pPrint guesses
--          putStrLn $ "Guesses: " ++ show guesses
  return ()
--  content <- readFile "input/day8.txt"
--  let lines' = lines content
--  let records = mapM parseLine lines'
--  case records of
--    Left e -> putStrLn $ "Failed to parse: " ++ show e
--    Right entries ->
--      let count' = countOf1478 entries in
--      putStrLn $ "Count of 1, 4, 7 or 8: " ++ show count'
