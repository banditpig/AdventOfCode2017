module Input (
    withData,
    parserDay1,
    parserDay2,
    parserDay4,
)
where
import           Data.Char
import           Text.Parsec
import           Text.Parsec.String (Parser, parseFromFile)

-- Make this into a library when done AoC
--
withData :: FilePath -> Parser a -> IO a
withData path p = do
    result <- parseFromFile (p <* eof) path
    either (error . show) return result


parserDay4 :: Parser [[String]]
parserDay4 = many1 (sepBy1 (many1 lower) (char ' ') <* newline)

parserDay2 :: Parser [[Int]]
parserDay2 = many1 (sepBy1 number (char '\t') <* newline)
    where
        number :: Parser Int
        number = read <$> many1 digit

parserDay1 :: Parser [Int]
parserDay1 = many1 (digitToInt <$> digit) <* newline




-- just tests.
day1 :: IO ()
day1 = withData "data/day01.txt" parserDay1 >>= \ d -> print d

day2 :: IO ()
day2 = withData "data/day02.txt" parserDay2 >>= \ d -> print d

day4 :: IO ()
day4 = withData "data/day04.txt" parserDay4 >>= \ d -> print d
