import           Data.List
import           Data.Maybe
newtype CF a = CF (Int -> a -> a)

evalCF :: (Eq a) => CF a -> Int -> a -> a
evalCF (CF f) = f
ints :: [Int]
ints  = [1,2,5,6,3,3,3,3,3,3,3,3,2,1]

f :: Int -> [Int] -> [Int]
f i  xs = if i > length xs then [-1] else [(!!) xs]

t = CF f
h = CF f
--                      data-tortoise-hare
checkCycle :: (Eq a) => a -> CF a -> CF a -> (Bool, Int, Int)
checkCycle = go 0 1  where
    go tix hix dta t h
     | tv == hv = (True, tix, hix)
     | otherwise = go (tix + 1) (hix + 2) dta t h where
        tv = evalCF t tix dta
        hv = evalCF h hix dta

