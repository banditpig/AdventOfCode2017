{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

module Day4 where
import           Data.List
import           Input

allUnique :: Eq a => [a] -> Bool
allUnique xs = xs == nub xs

main :: IO ()
main =
  withData "data/day04.txt" parserDay4 >>= \input -> do
    putStrLn "Day 4\n-----"
    -- part 1
    print $ length $ filter allUnique input
    --part 2.
    -- fmap (fmap sort) input => fmap (fmap sort) [[]]
    -- which runs sort over each string in each row. So then can just apply filter allUnique
    print $ length $ filter allUnique $ fmap (fmap sort) input
                                      -- or fmap sort <$> input

