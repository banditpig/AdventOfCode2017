{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import           Control.Monad
import           Data.List.Split
import qualified Data.Vector     as V
data Direction = Up | Down deriving (Eq, Show)

data Layer = Layer {
                    depth :: Int,
                    range :: Int,
                    pos   :: Int,
                    dir   :: Direction
                    } deriving Show

type Wall = V.Vector Layer

readLine :: String -> Layer
readLine s = l where
     [ds, rs] = splitOn ": " s
     l = Layer (read ds) (read rs) 0 Down

readAll :: [String] -> Wall
readAll xs = V.fromList (map readLine xs)

updatePos :: (Num a, Eq a) => a -> a -> Direction -> (a, Direction)
updatePos range pos dir
    | range == 1                          = (pos, dir)
    | pos   == (range - 1) && dir == Down = (pos - 1, Up)
    | pos   == 0           && dir == Up   = (pos + 1 , Down)
    | dir   == Up                         = (pos - 1, Up)
    | dir   == Down                       = (pos + 1, Down)
    | otherwise                           = (pos, dir)    -- won't happen :)


updateLayer :: Layer -> Layer
updateLayer Layer {..} = Layer depth range pos' dir' where
    (pos', dir') = updatePos range pos dir

updateWall :: Wall -> Wall
updateWall = V.map updateLayer

severity :: Int -> Wall -> Int
severity packetPos wall =
    case V.find (\ l -> depth l == packetPos) wall of
        Just Layer {..} -> if pos == 0 then depth * range else 0
        _               -> 0


runPacket end = go 0 0 where
    go pos sev wall
        | pos >= end = sev
        | otherwise = go (pos + 1) sev' wall' where
                        sev'  = sev + severity pos wall
                        wall' = updateWall wall

main :: IO ()
main = do
    input <- readFile "data/day13.txt"
    print $ runPacket 99  $  readAll . lines $  input

