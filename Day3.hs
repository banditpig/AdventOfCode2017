

data Direction = R | L | U | D deriving Show
type Move = (Int, Direction)
type Path = [Move]
type Location = (Int, Int)

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat $ zipWith (\ x y -> [x, y])  xs ys

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
generatePath = concatMap expand $ zip distanceList directionList where
    expand (count, d) = replicate count (1, d)

findEndLocation target = go 1 (0, 0) generatePath  where
    go n loc@(x,y) (p@(i,d):ps)
        | n >= target = loc
        | otherwise = go (n + i) (applyDirection loc d) ps

solveForDistance :: Int -> Int
solveForDistance target = dist (0,0) $ findEndLocation target
