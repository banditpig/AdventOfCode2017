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
type WeightMap = M.Map String Int

-- all c
canBeResolved :: WeightMap -> Tower -> Bool
canBeResolved mp (Tower _ childNames) = and . map (`M.member` mp)  $ childNames

hasChildren :: Tower -> Bool
hasChildren (Tower _ ts) = ts /= []
                    -- parents -- children
splitList :: [Tower] -> ([Tower], [Tower])
splitList ts = (filter hasChildren ts,filter (not . hasChildren) ts)

towerNames  :: [Tower] -> [String]
towerNames []                       = []
towerNames (Tower (name, _) _ : ts) = name : towerNames ts

childNames :: [Tower] -> [String]
childNames  []                           = []
childNames (Tower (name, _) towers : ts) =  towers ++ childNames ts


root :: [Tower] -> String
root ts = res where
    tn = nub . towerNames $ ts
    cn = nub . childNames $ ts
    res = head $ (\\) tn  cn

showTower :: [Tower] -> String
showTower [] = ""
showTower ( Tower (n, w) _ :ts) = show n ++ " " ++ show w ++ show " || " ++ showTower ts
main :: IO ()
main = do
    withData "data/day07.txt" parserDay7 >>= \ towerList -> do
        let (p, c ) = splitList towerList
       -- print $ allWeights towerList (weightMap towerList)
        print $ root towerList

        --print $ showTower p
        -- ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
        -- padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
        -- fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243

