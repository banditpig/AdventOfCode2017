{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import           Control.Monad
import           Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

type Key = String

decode :: String -> [String]
decode  s = join $ splitOn " <-> " <$> splitOn ", " s

fullDecode :: [String] -> [[String]]
fullDecode xs = decode <$> xs

toMap :: M.Map String [String] -> [[String]] -> M.Map String [String]
toMap = foldl (\ m x -> M.insert (head x) (tail x) m)

neighbours :: Key -> M.Map String [String] -> S.Set String
neighbours key m = go S.empty [key] where
  go set []      = set
  go set (x:xs)  = go (S.insert x set) (q ++ xs) where
    q = if S.member x set then [] else  m  M.! x

main :: IO ()
main = do
    input <- readFile "data/day12.txt"
    -- part 1 - 141
    print $  S.size . neighbours "0" $ toMap M.empty  (fullDecode . lines $ input)

