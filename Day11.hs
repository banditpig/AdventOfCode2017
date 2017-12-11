import           Data.List
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

distance :: Hex -> Int
distance (x, y, z)  = (abs x + abs y + abs z) `div` 2

update ::  Hex ->  String -> Hex
update loc m =  move  (strToDiv  m) loc

main = do
    input <- readFile "data/day11.txt"
    -- part 1 - 643.
    print $ distance . foldl' update start $ splitOn "," input
    -- part 2 - 1471.
    print $ maximum . map distance . scanl' update start $ splitOn "," input

