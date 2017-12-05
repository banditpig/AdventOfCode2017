{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import           Control.Monad.State
import qualified Data.Map.Strict     as M
import           Input

type JumpState = (M.Map Int Int, Int)
type JumpFunc = Int -> Int



jumpPart1 :: JumpFunc
jumpPart1 = (1+)

jumpPart2 :: JumpFunc
jumpPart2 i = if i >= 3 then i -1 else i + 1

makeMap :: [Int] -> M.Map Int Int
makeMap d = M.fromList $ zip  [1..] d

solve :: JumpFunc ->  State JumpState Int
solve jf = go 1 where
    go :: Int -> State JumpState Int
    go i = do
        (map, cnt) <- get
        let step = map M.! i
            newi = i + step
            newval = jf step
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
    print $ evalState (solve jumpPart1) (makeMap input, 0)
    -- part 2 = 27688760 - might need to use ghc rather than ghci so that 'solve' can be tail call optimised.
    print $ evalState (solve jumpPart2) (makeMap input, 0)

