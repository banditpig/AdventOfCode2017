{-# LANGUAGE TupleSections #-}
module Input
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

-- this wont do -ve
parserDay1 :: Parser [Int]
parserDay1 = many1 (digitToInt <$> digit) <* newline
-- this will do -ve
parserDay5 :: Parser [Int]
parserDay5 = many ( int <* newline)

int :: Parser Int
int = char '-' *> pure ((-1)*) <*> natural <|> natural

natural :: Parser Int
natural = pure read <*> many1 digit

type Program = (String , Int)
data Tower = Tower (String , Int) [Tower]

instance Show Tower where
    show ( Tower nw towers) = show nw ++ show " -> " ++ show (map show towers)
instance Eq  Tower where
    (==) (Tower (n, _) _) (Tower (n', _) _)  = n == n'


createDummyNode :: String -> Tower
createDummyNode s = Tower (s,0) []

commaSp :: Parser String
commaSp = string ", "

names :: Parser [Tower]
names = do
    nl <- sepEndBy1 (many1 letter) commaSp
    return $ createDummyNode <$> nl

program :: Parser Program
program = do
    n <- many1 letter
    space
    char '('
    w <- natural
    char ')'
    return (n, w)

-- see option https://hackage.haskell.org/package/parsec-3.1.11/docs/Text-ParserCombinators-Parsec-Combinator.html
progLine :: Parser Tower
progLine = do
    p <- program
    arrow <- optionMaybe (string " -> ")
    case arrow of
        Just arrow -> do
            ns <- names
            return ( Tower p ns)
        _ -> return ( Tower p [])

allTowers = many (progLine <* newline)
parserDay7 = allTowers
-- just tests.
day1 :: IO ()
day1 = withData "data/day01.txt" parserDay1 >>= \ d -> print d

day2 :: IO ()
day2 = withData "data/day02.txt" parserDay2 >>= \ d -> print d

day4 :: IO ()
day4 = withData "data/day04.txt" parserDay4 >>= \ d -> print d

day7 :: IO ()
day7 = withData "data/day07.txt" allTowers >>= \ d -> print d
