
module Day1 where
import           Data.Char
import           Input

-- build up pairs, pairing defined by an offset.
doPairs :: Int -> [Int] -> [(Int, Int)]
doPairs offset  xs = [(xs !! i, xs !! ((i + offset) `mod` length xs)  ) | i <- [0.. length xs - 1] ]

-- fold over the pairs
getSum :: [(Int, Int)] -> Int
getSum = foldr f 0 where f (x,y) a = if x == y then a + x else a

solveWith :: Int -> [Int] -> Int
solveWith offset = getSum . doPairs offset

main :: IO ()
main =
  withData "data/day01.txt" parserDay1 >>= \ str -> do
    putStrLn "Day 1\n-----"
    -- part 1, pairs are adjacent
    print $ solveWith 1 str
    -- part 2, pairs are half way around
    print $ solveWith (length str `div` 2) str
