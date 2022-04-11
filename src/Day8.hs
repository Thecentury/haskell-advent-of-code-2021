module Day8 where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec
import Text.Parsec.Char (char, digit, string, letter, space)
import Control.Monad (void)

--------------------------------------------------------------------------------
-- unique signal patterns | four digit output value
-- dac abcf ac fdbcga dgcbae gcbfde fgcbd agfed adcgf cdbgfea | cbfa bcafdg cbfa bafcgd

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

recordParser :: Parser ([String], [String])
recordParser =
  do
    signalPatterns <- wordsParser
    void (string " | " <?> "delimiter")
    outputValues <- wordsParser
    eof
    return (signalPatterns, outputValues)

parseLine :: String -> Either ParseError ([String], [String])
parseLine = parse recordParser ""

--------------------------------------------------------------------------------

run :: IO ()
run = do
  content <- readFile "input/day8.txt"
  let lines' = lines content
  let records = mapM parseLine lines'
  putStrLn $ "Records: " ++ show records

  return ()
