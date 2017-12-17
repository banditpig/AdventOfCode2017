import           Data.List
import           Data.List.Split
import           Data.Maybe      (fromMaybe)
import qualified Data.Vector     as V

spin :: Int -> V.Vector a -> V.Vector a
spin i v = (V.++) u  v' where
    sx = V.length v - i
    (v', u) = V.splitAt sx v

exch :: Int -> Int -> V.Vector a -> V.Vector a
exch i j v = (V.//) v [(i, (V.!) v j), (j, (V.!) v i)]

part :: (Eq a) => a -> a -> V.Vector a -> V.Vector a
part x y v = exch ix iy  v where
    ix = fromMaybe 0 $ V.elemIndex x v
    iy = fromMaybe 0 $ V.elemIndex y v

oneInstr :: String -> (V.Vector Char -> V.Vector Char)
oneInstr (x:xs)
    | x == 's' = spin (read xs :: Int)
    | x == 'x' = exch c1' c2'
    | x == 'p' = part (head c1) (head c2)
      where [c1, c2]   = splitOn "/" xs
            [c1', c2'] = read <$> [c1, c2]


applyFs :: [V.Vector Char -> V.Vector Char] -> V.Vector Char  -> V.Vector Char
applyFs fs v = foldl' (\ v f -> f v) v fs

cycl :: V.Vector Char -> V.Vector Char -> [V.Vector Char -> V.Vector Char] -> [V.Vector Char]
cycl t v fs = v : takeWhile (/= t) (iterate (applyFs fs) (applyFs fs v))

part2 :: [V.Vector Char -> V.Vector Char] -> V.Vector Char -> V.Vector Char
part2 fs v = res where
     cyc = cycl v v fs
     (_, ix) =  1000000000 `divMod` length cyc
     res = (!!) cyc ix

main = do
    input <- splitOn "," <$> readFile "data/day16.txt"
    --part 1 fgmobeaijhdpkcln
    print $ applyFs (oneInstr <$> input) (V.fromList ['a' .. 'p'])
    let inV = V.fromList ['a'..'p']
    let fs =  oneInstr <$> input

    -- part 2 lgmkacfjbopednhi
    print $ part2 fs inV

