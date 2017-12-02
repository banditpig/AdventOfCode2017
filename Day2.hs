module Day2 where
import           Input

testInput = [[5,9,2,8],
            [9,4,7,3],
            [3,8,6,5]]

minMaxDif :: [Int] -> Int
minMaxDif r = ma - mi  where (mi, ma) = (minimum r, maximum r)

evenDiv :: [Int] -> Int
evenDiv r =  head [ x `div` y | x <- r, y <- r, x `mod` y == 0, x /= y]

foldWithFunc :: (Num b) => (a -> b) -> [a] -> b
foldWithFunc f = foldr (\r a -> a + f r) 0

evenDivSum :: [[Int]] -> Int
evenDivSum  = foldWithFunc evenDiv

minMaxSum :: [[Int]] -> Int
minMaxSum  = foldWithFunc minMaxDif

main :: IO ()
main =

    withData "data/day02.txt" parserDay2 >>= \ input -> do
        putStrLn "Day 2\n-----"
        -- part 2
        print $ minMaxSum input
        -- part 2
        print $ evenDivSum input
