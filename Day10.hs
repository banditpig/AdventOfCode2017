{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
import           Data.Bits
import           Data.Char   (ord)
import           Data.List
import qualified Data.Vector as V
import           Numeric

atIndexC :: V.Vector a -> Int -> a
atIndexC v i
    | i < 0 = atIndexC v (V.length v + i)
    | otherwise = (V.!) v (i `mod` V.length v )

sliceC :: Int -> Int  -> V.Vector  a -> V.Vector a
sliceC ix l v
    | ix + l <= V.length v = V.slice ix l v
    | otherwise = V.slice ix l' v  V.++  V.slice 0 (l - l') v where
         vl = V.length v
         l' = vl - ix

updateC :: V.Vector a -> V.Vector (Int, a) -> V.Vector a
updateC v ps = doPairs (V.toList ps) v where
  doPairs [] v = v
  doPairs ((ix, x):xs) v = doPairs xs (V.update v (V.fromList [ (ix `mod` V.length v, x)]))


revAndPair :: V.Vector a -> Int -> Int -> V.Vector (Int, a)
revAndPair v ix len = V.zipWith (,) (V.fromList [(ix + x) `mod` len | x <- [0.. length v]]) (V.reverse v)

startV :: V.Vector Int
startV =  V.fromList [0..255]

lens :: [Int]
lens = [94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243]
lens2 :: String
lens2 = "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"

processInput ::  Int -> Int ->  V.Vector Int -> [Int] -> (V.Vector Int, Int, Int)
processInput ix skip v [] = (v, ix, skip)
processInput  ix skip startListV (len:lens) = processInput ix' (skip + 1 ) startListV' lens where
    slice = sliceC ix len startListV
    revPairs = revAndPair slice ix (length startListV)
    ix' = (ix + len + skip ) `mod` length startListV
    startListV' = updateC startListV revPairs

input2 = (ord <$> lens2) ++ [17, 31, 73, 47, 23]
sparseHash 0 _ _ v _ = v
sparseHash cnt ix skip startV lens = sparseHash (cnt - 1) ix' skip' newv lens  where
     (newv, ix', skip') = processInput ix skip startV lens

denseHash :: V.Vector Int -> String
denseHash sparse = res where
    list = grp 16 (V.toList sparse)
    dense = [ foldr xor  (head row) (tail row) | row <- list]
    res = concatMap (pad . (`showHex` "")) dense

pad [c] = ['0', c]
pad cs  = cs
grp :: Int -> [a] -> [[a]]
grp _ [] = []
grp n l
  | n > 0 = take n l : grp n (drop n l)
  | otherwise = error "Negative n"

main = do
    -- part 1 [23715]
    let (resV, _, _) = processInput  0 0 startV lens
    print $ (*) <$> [ (V.!) resV 0] <*> [(V.!) resV 1]

    -- part 2 "541dc3180fd4b72881e39cf925a50253"
    let spa = sparseHash 64 0 0 (V.fromList [0..255]) input2
    print $ denseHash spa
