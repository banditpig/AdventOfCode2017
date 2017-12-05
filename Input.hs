module Input (
    withData,
    parserDay1,
    parserDay2,
    parserDay4,
    parserDay5,

)
where
import           Data.Char
import           Text.Parsec
import           Text.Parsec.String

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

parserDay5 :: Parser [Int]
parserDay5 = many ( (int) <* newline)


natural :: Parser Int
natural = pure read <*> many1 digit



int :: Parser Int
int = char '-' *> pure ((-1)*) <*> natural <|> natural


-- just tests.
day1 :: IO ()
day1 = withData "data/day01.txt" parserDay1 >>= \ d -> print d

day2 :: IO ()
day2 = withData "data/day02.txt" parserDay2 >>= \ d -> print d

day4 :: IO ()
day4 = withData "data/day04.txt" parserDay4 >>= \ d -> print d
