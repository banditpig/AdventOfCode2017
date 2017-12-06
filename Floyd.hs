
newtype CF a = CF (Int -> a -> a)

evalCF :: (Eq a) => CF a -> Int -> a -> a
evalCF (CF f) i x  = f i x

t :: (Eq a) => CF a -> Int -> a -> a
t = evalCF

h :: (Eq a) =>  CF a -> Int -> a -> a
h cf i = evalCF cf (i + 1)

checkCycle :: (Eq a) => a -> CF a -> CF a -> Bool
checkCycle dta t h = go 0 dta t h where
    go ix dta t h
     | tv == hv = True
     | otherwise = go (ix + 1) dta t h where
        tv = evalCF t ix dta
        hv = evalCF t (ix + 1) dta

