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
              | Const Int  deriving Show

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
transformExpr (PLeaf (Number, str)) = Const (read str::Int)
transformExpr (PLeaf (Variable, str)) = Load str
transformExpr (PNode ArExpr (e1:aop:e2:[])) = case aop of
                                          PLeaf (ArOp, op) -> BinExpr (toOp op) (transformExpr e1) (transformExpr e2)
transformExpr (PNode CompExpr (e1:aop:e2:[])) = case aop of
                                                  PLeaf (CompOp, op) -> BinExpr (toOp op) (transformExpr e1) (transformExpr e2)
transformExpr (PNode SimExpr (e:[])) = transformExpr e



-- == Tests ==
testPrpr p = prpr $ parse grammar Program $ tokenizer p
testGr   p = showRoseTree $ toRoseTree $ parse grammar Program $ tokenizer p
testTrf  p = transform $ parse grammar Program $ tokenizer p
prog1 = "i=0 loop 10 i=(i+1) i=(i+2) "
prog2 = "i = 0 if (i > 0) { if (i<=0) i = (i-1) } else i=10"
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

