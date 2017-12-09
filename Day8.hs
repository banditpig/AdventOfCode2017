{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import qualified Data.Map.Strict as M
import           Data.Maybe
import           Debug.Trace

type Regs   = M.Map String Int

data BoolOp  = LThan | LThanEq | GThan | GThanEq | Eqq | NotEq deriving (Show,Eq)
data NumOp   = Inc | Dec deriving (Show, Eq)
newtype NumVal = Number Int deriving Show
newtype RegVal = Reg String  deriving Show
data IfStmt  = IF RegVal BoolOp NumVal  deriving Show
data NumExpr = NUM RegVal NumOp NumVal  deriving Show

makeExpr :: [String] -> Expr
makeExpr [reg1, numop, numbr1, "if", reg2, boolop, numbr2] = expr where
    reg1'   = Reg reg1
    numop'  = getNumOp numop
    numbr1' = Number (read numbr1)
    reg2'   = Reg reg2
    boolop' = getBoolOp boolop
    numbr2' = Number (read numbr2)
    expr    = Expr (NUM reg1' numop' numbr1') (IF reg2' boolop' numbr2')

data Expr    = Expr NumExpr IfStmt  deriving Show

lookupReg :: Regs -> String -> Int
lookupReg regs r = fromMaybe (error $ "Unknown Reg " ++ show r) (M.lookup r regs)

evalBool :: IfStmt -> Regs -> Bool
evalBool (IF (Reg r) op (Number n)) regs = bool op (lookupReg regs r ) n where
     bool op v v'
        | op == LThan   = v <  v'
        | op == LThanEq = v <= v'
        | op == GThan   = v >  v'
        | op == GThanEq = v >= v'
        | op == Eqq     = v == v'
        | op == NotEq   = v /= v'


evalNum :: NumExpr -> Regs -> Regs
evalNum (NUM (Reg r) op (Number n)) regs
    | op == Inc = inc r n regs
    | otherwise = dec r n regs

evalExpr (Expr numExpr ifStmt ) regs = if evalBool ifStmt regs then evalNum numExpr regs else regs

inc :: String -> Int -> Regs -> Regs
inc r n regs = regs' where
    n' = trace ("inc->" ++ show n) $ n + lookupReg regs r
    tempReg =  if lookupReg regs "maxVal" < n'
               then M.insert "maxVal" n' regs
               else regs
    regs' = M.insert r n' tempReg

dec :: String -> Int -> Regs -> Regs
dec r n regs = regs' where
    n' =  lookupReg regs r - n
    tempReg =  if lookupReg regs "maxVal" < n'
               then M.insert "maxVal" n' regs
               else regs
    regs' = M.insert r n' tempReg

getNumOp :: String -> NumOp
getNumOp op
    | op == "inc" = Inc
    | otherwise = Dec
getBoolOp :: String -> BoolOp
getBoolOp op = case op of
    "<"  -> LThan
    "<=" -> LThanEq
    ">"  -> GThan
    ">=" -> GThanEq
    "==" -> Eqq
    "!=" -> NotEq


parseLine :: String -> Expr
parseLine ln = makeExpr (words ln)

parseInput :: [String] -> [Expr]
parseInput = foldr (\ x ac -> parseLine x : ac) []

makeRegs :: Regs -> [String] -> Regs
makeRegs  regs []    = regs
makeRegs regs (x:xs) = makeRegs (M.insert (head (words x)) 0 regs) xs

evalProg :: Regs -> [Expr] -> Regs
evalProg  = foldl (flip evalExpr)

main :: IO ()
main = do
    input <- readFile "data/day08.txt"
    let regs = makeRegs M.empty (lines input)
    let prog  = parseInput (lines input)

    print $ lines input
    print $  evalProg (M.insert "maxVal" 0 regs)  prog

