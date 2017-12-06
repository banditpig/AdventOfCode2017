
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import           Data.List
import qualified Data.Sequence as S

signature :: S.Seq Int -> String
signature = foldr ((++) . show) ""

pair :: S.Seq Int  -> (Int, Int)
pair xs = (m, i) where
    m = maximum xs
    Just i = S.elemIndexL m xs

modList :: Int -> S.Seq a -> Int
modList ix s = ix `mod` S.length s

distribute :: (Int, Int) -> S.Seq Int -> S.Seq Int
distribute (v, ix) ints = go (v, modList (ix + 1) ints  ) (S.update (modList ix ints) 0 ints)  where
    go :: (Int, Int) -> S.Seq Int -> S.Seq Int
    go (0, _) xs = xs
    go (v, ix) xs = go  (v - 1, modList (ix + 1) xs) newXs where
        nv = S.index  xs ix
        newXs = S.update ix (nv + 1) xs


solve :: S.Seq Int -> (Int, String, [String])
solve = go 0 []  where
    go count sigs ints
     | thisSig `elem` sigs = (count, thisSig, sigs)
     | otherwise = go  (count + 1) ( thisSig :sigs) ( distribute (pair ints) ints)
       where thisSig = signature ints

testData :: [Int]
testData = [0,2,7,0]

main :: IO ()
main = do
    --
    putStrLn "Day 6\n-----"
    let (cnt, target, lst) = solve $ S.fromList [10,3,15,10,5,15,5,15,9,2,5,8,5,2,3,6] --
    -- part 1 - 14029
    print cnt
    -- part 2 - Just 2765
    print $ (+1) <$> elemIndex target lst
