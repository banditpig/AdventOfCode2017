{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import           Control.Monad.State
import qualified Data.Map.Strict     as M
import           Data.Maybe
data Direction = R | L | U | D deriving (Show, Eq)
type Move = (Int, Direction)
type Path = [Move]
type Location = (Int, Int)

type LocationValue = Int
type AllLocation = M.Map Location LocationValue


interleave :: [a] -> [a] -> [a]
interleave xs ys = concat $ zipWith (\ x y -> [x, y])  xs ys

addLocations :: Location -> Location -> Location
addLocations (x, y) (x1, y1) = (x + x1, y + y1)

dist :: Location -> Location -> Int
dist (x1, y1) (x2, y2) = abs (x1 -  x2) + abs (y1 -  y2)

applyDirection :: Location -> Direction -> Location
applyDirection (x, y) d =
    case d of
        R -> (x + 1, y)
        L -> (x - 1, y)
        U -> (x    , y - 1)
        D -> (x    , y + 1)

-- i.e. [R, U, L, D,R, U, L, D...]
directionList :: [Direction]
directionList = concat . repeat $ [R, U, L, D]

-- i.e. [1,1,2,2,3,3,4,4...]
distanceList = 1 : interleave z z' where
    z  = [1,2..]
    z' = tail z

-- ie [(1,R),(1,U),(1,L),(1,L),(1,D),(1,D),(1,R),(1,R),(1,R),(1,U)...] created by
-- expanding [(1,R),(1,U),(2,L),(2,D),(3,R),(3,U),(4,L),(4,D),(5,R),(5,U)...
generatePath :: Path
generatePath = concatMap expand $ zip distanceList directionList where
    expand (count, d) = replicate count (1, d)

findEndLocation target = go 1 (0, 0) generatePath  where
    go n loc (p@(i,d):ps)
        | n >= target = loc
        | otherwise = go (n + i) (applyDirection loc d) ps

-- part 1
solveForDistance :: Int -> Int
solveForDistance target = dist (0,0) $ findEndLocation target


-- find neighbours of Location and, if present in Map, add up values
valueForLocation :: Location -> AllLocation -> Int
valueForLocation location  locations  = foldr f 0  neighbs  where
    -- add the current loc to all the (relative) neighbours to give the absolute neighbours
    -- (this could be more efficient as only half are meaninful depending of direction)
    neighbs = addLocations location <$> [ (x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1]]
    -- if this loc is in the (map of) locations add its value
    f loc acc  = acc + M.findWithDefault 0 loc locations

--What is the first value written that is larger than your puzzle input?
sumOverPath :: Int ->  Location -> Path -> State AllLocation Int
sumOverPath target currentLoc ((_, d):ps) = do
    locationsMap <- get
    let nextLoc = applyDirection currentLoc d
    let v = valueForLocation nextLoc locationsMap
    put $ M.insert nextLoc v locationsMap
    if v > target
        then return v
    else
        sumOverPath target nextLoc ps

part2 :: Int -> Int
part2 target = evalState (sumOverPath target (0,0) generatePath) $ M.singleton  (0, 0) 1
