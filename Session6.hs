module Session6 where

import FPPrac.Trees
import GHC.Generics

import FP_TypesEtc
import FP_ParserGen (parse)

import Tokenizer

--Exercise 1/2
grammar :: Grammar

grammar nt = case nt of
            Program  -> [ [(+:) [Stmnt]] ]
            Expr     -> [ [SimExpr], [ArExpr], [CompExpr] ]
            SimExpr  -> [ [nmbr], [var] ]
            ArExpr   -> [ [lBracket, Expr, arop, Expr, rBracket] ]
            CompExpr -> [ [lBracket, Expr, compop, Expr, rBracket] ]
            Stmnt    -> [ [var, assignop, [ArExpr] <> [SimExpr]], 
                          [rep, Expr, Block],
                          [_if, CompExpr, Block, (?:) [_else, Block ]] ]
            Block    -> [ [lBrace, (*:) [Stmnt], rBrace], [Stmnt] ]

lBracket = Symbol "("
rBracket = Symbol ")"
lBrace   = Symbol "{"
rBrace   = Symbol "}"

nmbr     = SyntCat Number
var      = SyntCat Variable
arop     = SyntCat ArOp
compop   = SyntCat CompOp
assignop = SyntCat AssignOp
rep      = SyntCat KW_Repeat
_if      = SyntCat KW_If
_else    = SyntCat KW_Else

--Exercise 3
type EVar     = String 
data Op       = Add | Subtr | Mult | Lesser | Greater | LE | GE | Equal deriving Show
data EStat    = Assign EVar EExpr
              | Repeat EExpr [EStat]
              | If EExpr [EStat] ElsePart  deriving Show
data ElsePart = Else [EStat]  deriving Show
data EExpr    = BinExpr Op EExpr EExpr
              | Load EVar
              | ConstInt Int  
              | ConstReal Double 
                deriving Show

lut :: String -> Int
lut "sum" = 0
lut "i"   = 1

toOp :: String -> Op
toOp "+" = Add
toOp "-" = Subtr
toOp "*" = Mult
toOp "<" = Lesser
toOp ">" = Greater
toOp "<="= LE
toOp ">="= GE
toOp "=="= Equal

convert :: String -> EExpr
convert (x:xs) | x == '~' && '.' `elem` (x:xs) = ConstReal ((negate $ read (xs))::Double)
               | x == '~' = ConstInt ((negate $ read (xs))::Int)
               | '.' `elem` (x:xs) = ConstReal ((read (x:xs))::Double)
               | otherwise = ConstInt ((read (x:xs))::Int)

transform :: ParseTree -> [EStat]
transform (PNode Program ss) = (map transformStats ss)

transformStats :: ParseTree -> EStat
transformStats (PNode Stmnt (i:c:s1:e:s2:[])) = If (transformExpr c) (transformBlock s1) (Else (transformBlock s2))
transformStats (PNode Stmnt (l:m:r:[])) = case l of
                                    PLeaf (Variable, str) -> Assign str (transformExpr r)
                                    PLeaf (KW_Repeat, _)  -> Repeat (transformExpr m) (transformBlock r)
                                    PLeaf (KW_If, _)      -> If (transformExpr m) (transformBlock r) (Else [])

transformBlock :: ParseTree -> [EStat]
transformBlock (PNode Block xs) = map transformStats xs

transformExpr :: ParseTree -> EExpr
transformExpr (PNode Expr (e:[])) = transformExpr e
transformExpr (PLeaf (Number, str)) = convert str
transformExpr (PLeaf (Variable, str)) = Load str
transformExpr (PNode ArExpr (e1:aop:e2:[])) = case aop of
                                          PLeaf (ArOp, op) -> BinExpr (toOp op) (transformExpr e1) (transformExpr e2)
transformExpr (PNode CompExpr (e1:aop:e2:[])) = case aop of
                                                  PLeaf (CompOp, op) -> BinExpr (toOp op) (transformExpr e1) (transformExpr e2)
transformExpr (PNode SimExpr (e:[])) = transformExpr e

--Exercise 4b
{-
The extra instruction are compare and various jump instructions.
The compare instruction will compare two values from the stack
and either sets a tuple with flags (which has to be returned
together with stack/heap etc.) or will set a value on the stack
that represents the flags in binary (with each bit representing a
flag). For instance: 110b = Lesser and Equal and 001b is Greater.
The jump instruction takes a value from the stack and either sets 
the PC to that value when the appropriate flag is set or increments
the PC when the flag is not set. 
-}

-- == Tests ==
testPrpr p = prpr $ parse grammar Program $ tokenizer p
testGr   p = showRoseTree $ toRoseTree $ parse grammar Program $ tokenizer p
testTrf  p = transform $ parse grammar Program $ tokenizer p
prog1 = "i=0 loop 10 i=(i+1.5) i=(i+2.5) "
prog2 = "i = 0 if (i > ~3) { if (i<=0) i = (i+~1) } else i=10"
prog3 = "sum=0i=0loop 10 {i=(i+1)sum=(sum+i)}"

test11 = testPrpr prog1
test12 = testGr prog1
test13 = testTrf prog1
test21 = testPrpr prog2
test22 = testGr prog2
test23 = testTrf prog2
test31 = testPrpr prog3
test32 = testGr prog3
test33 = testTrf prog3

