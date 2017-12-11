import           Data.List.Split

type X = Int
type Y = Int
type Z = Int

data Dir = N | NE  | SE | S | SW | NW

type Hex = (X, Y, Z)

start :: Hex
start = (0, 0, 0)

move :: Dir -> Hex -> Hex
move d (x, y, z)  = case d of
    N  -> (x, y + 1, z - 1)
    NE -> (x + 1, y, z - 1)
    SE -> (x + 1, y - 1, z)
    S  -> (x, y - 1, z + 1)
    SW -> (x - 1, y, z + 1)
    NW -> (x - 1, y + 1, z)

strToDiv :: String -> Dir
strToDiv str = case str of
    "n"  -> N
    "s"  -> S
    "ne" -> NE
    "nw" -> NW
    "sw" -> SW
    "se" -> SE

distance :: Hex -> Hex -> Int
distance (x, y, z) (x', y', z') = (abs (x - x') + abs (y - y') + abs (z - z')) `div` 2

update ::  (Hex, Int) ->  String -> (Hex, Int)
update (loc, dist) m = (loc', dist') where
       loc' = move  (strToDiv  m) loc
       newDist = distance loc' (0,0,0)
       dist' = if newDist > dist then newDist else dist


main = do
    input <- readFile "data/day11.txt"

    let (end, d) = foldl update (start, 0) $ splitOn "," input
    -- part 1 - 643.
    print $  distance end (0,0,0)
    -- part 2 - 1471.
    print d

