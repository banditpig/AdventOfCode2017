{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Input

{-
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)

               gyxo
              /
         ugml - ebii
       /      \
      |         jptl
      |
      |         pbga
     /        /
tknk --- padx - havc
     \        \
      |         qoyq
      |
      |         ktlj
       \      /
         fwft - cntj
              \
                xhth

-}


hasChildren :: Tower -> Bool
hasChildren (Tower _ ts) = ts /= []

                    -- parents -- children
splitList :: [Tower] -> ([Tower], [Tower])
splitList ts = (filter hasChildren ts,filter (not . hasChildren) ts)

weightMap :: [Tower] -> M.Map String Int
weightMap = foldr f M.empty where
    f (Tower (name, weight) _) = M.insert name weight

towerWeight :: M.Map String Int -> Tower ->  (String, Int)
towerWeight mp (Tower (name, w) towers) = (name, sumWeight) where
    sumWeight = foldr (getWeight mp)  w towers
    getWeight mp (Tower (name, _) _ ) acc  = acc +  M.findWithDefault (-999) name mp

    towerNames  :: [Tower] -> [String]
towerNames []                       = []
towerNames (Tower (name, _) _ : ts) = name : towerNames ts

childNames :: [Tower] -> [String]
childNames  []                           = []
childNames (Tower (name, _) towers : ts) = towerNames towers ++ childNames ts

allWeights :: [Tower] -> M.Map String Int -> [(String, Int)]
allWeights ts mp = foldr (\t ac -> towerWeight mp t : ac) [] ts


root :: [Tower] -> String
root ts = res where
    tn = nub . towerNames $ ts
    cn = nub . childNames $ ts
    res = head $ (\\) tn  cn

main :: IO ()
main = do
    withData "data/day07.txt" parserDay7 >>= \ towerList -> do
        let (p, c ) = splitList towerList
        print $ allWeights towerList (weightMap towerList)
        print $ root towerList


