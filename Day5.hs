{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import           Control.Monad.State
import qualified Data.Map.Strict     as M
import           Input

makeMap :: [Int] -> M.Map Int Int
makeMap d = M.fromList $ zip  [1..] d

type JumpState = (M.Map Int Int, Int)

solve :: State JumpState Int
solve = go 1 where
    go :: Int -> State JumpState Int
    go i = do
        (map, cnt) <- get
        let step = map M.! i
            newi = i + step
            newval = step + 1
        if newi > M.size map
            then return (cnt + 1)
            else do
                put (M.insert i newval map, cnt + 1)
                go newi

testMap ::  M.Map Int Int
testMap = M.fromList [(1,0),(2,3),(3,0),(4,1),(5,-3)]

main :: IO ()
main =
    withData "data/day05.txt" parserDay5 >>= \ input -> do
        putStrLn "Day 5\n-----"
        -- part 1 = 359348
        print $ evalState solve (makeMap input, 0)

