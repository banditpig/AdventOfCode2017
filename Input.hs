module Input where
import           Data.Char
import           Text.Parsec
import           Text.Parsec.String (Parser, parseFromFile)

withData :: FilePath -> Parser a -> IO a
withData path p = do
    result <- parseFromFile (p <* eof) path
    either (error . show) return result

parserDay2 :: Parser [[Int]]
parserDay2 = many1 (sepBy1 number (char '\t') <* newline)
    where
        number :: Parser Int
        number = read <$> many1 digit

parserDay1 :: Parser [Int]
parserDay1 = many1 (digitToInt <$> digit) <* newline

day1 :: IO ()
day1 = withData "../data/day01.txt" parserDay1 >>= \ d -> print d

day2 :: IO ()
day2 = withData "../data/day02.txt" parserDay2 >>= \ d -> print d

-- day02 :: IO ()
-- day02 =
--   withInput "input/2.txt" parser >>= \parsed -> do
--     putStrLn "Part 1"
--     print $ checksumOn difference parsed
--     putStrLn "Part 2"
--     print $ checksumOn evenDivision parsed
