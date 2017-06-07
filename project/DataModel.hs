{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module DataModel where

data Atom = A0 | A1 | A2 | B0 | B1 | B2 | C0 | C1 | D | A | B | C | Pr String Term deriving (Show, Eq)
data Term = Const Atom | Var String deriving (Show, Eq)
type Substitution = (Term, Term)
type Clause = (Atom, [Atom])
type Program = [Clause]
type Query = [Atom]

class Expr a where
    (⇐) :: a -> Substitution -> a

instance Expr Atom where
    (⇐) (Pr p a) s = Pr p (a ⇐ s)
    (⇐) a _ = a

instance Expr Term where
    (⇐) (Const a) _ = Const a
    (⇐) (Var v) (Var w, sub) | v == w = sub
                             | otherwise = Var v

instance Expr Clause where
    (⇐) (a, as) s =  (a ⇐ s, map (⇐s) as)



program :: Program
program = [(A0, []), 
           (A1, []), 
           (A2, []), 
           (B0, [A0,A1]),
           (B1, [A1,A2]),
           (B2, [A1,A2,D]),
           (C0, [B0,B1]),
           (C1, [B0,B1,B2]),
           (Pr "p" (Const A), []),
           (Pr "p" (Const B), []),
           (Pr "p" (Const C), []),
           (Pr "q" (Const A), []),
           (Pr "q" (Const B), []),
           (Pr "z" (Const D), []),
           (Pr "r" (Var "X"), [Pr "p" (Var "X"), Pr "q" (Var "X")])
--          ,(Pr "r" (Var "Y"), [Pr "p" (Var "Y")])
--          ,(Pr "r" (Var "Z"), [Pr "p" (Var "Z"), Pr "p" (Const A)])
--          ,(Pr "r" (Var "A"), [Pr "p" (Var "A"), D]) 
           ]
