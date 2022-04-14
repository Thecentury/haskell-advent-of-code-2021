module Day8 where

import Text.Parsec
import Text.ParserCombinators.Parsec
import Control.Monad (void)

--------------------------------------------------------------------------------
-- unique signal patterns | four digit output value
-- dac abcf ac fdbcga dgcbae gcbfde fgcbd agfed adcgf cdbgfea | cbfa bcafdg cbfa bafcgd

type OutputValue = String

data Entry = Entry {
  signalPatterns :: [String],
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
    return $ Entry signalPatterns outputValues

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

run :: IO ()
run = do
  content <- readFile "input/day8.txt"
  let lines' = lines content
  let records = mapM parseLine lines'
  case records of
    Left e -> putStrLn $ "Failed to parse: " ++ show e
    Right entries ->
      let count' = countOf1478 entries in
      putStrLn $ "Count of 1, 4, 7 or 8: " ++ show count'
