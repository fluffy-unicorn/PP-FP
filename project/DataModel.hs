{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module DataModel where

-- An atom can either be a property (e.g. A0), a uni-variable predicate (e.g. p(X)) or a multi-variable predicate (e.g. p(X,Y,Z))
data Atom = A0 | A1 | A2 | B0 | B1 | B2 | C0 | C1 | D | A | B | C_ | P String Term | PMult String [Term] deriving (Show, Eq)
-- A term consist of constants or variables
data Term = C Atom | V String deriving (Show, Eq)

type Substitution = (Term, Term)
type Clause = (Atom, [Atom])
type Program = [Clause]
type Query = [Atom]
type Solution = (Query, [Substitution])
type Dictionary = [(Term, [Term])]

-- Class and instance declarations for the substitute operator
class Expr a where
    (⇐) :: a -> Substitution -> a

instance Expr Atom where
    (⇐) (P p a) s = P p (a ⇐ s)
    (⇐) (PMult p terms) s = PMult p (map (⇐s) terms)
    (⇐) a _ = a

instance Expr Term where
    (⇐) (C a) _ = C a
    (⇐) (V v) (V w, sub)| v == w = sub
                        | otherwise = V v
    (⇐) (V v) (sub, V w)| v == w = sub
                        | otherwise = V v 

instance Expr Clause where
    (⇐) (a, as) s =  (a ⇐ s, map (⇐s) as)


--The same program in Prolog syntax is added below
program :: Program
program = [(A0, []), 
           (A1, []), 
           (A2, []), 
           (B0, [A0,A1]),
           (B1, [A1,A2]),
           (B2, [A1,A2,D]),
           (C0, [B0,B1]),
           (C1, [B0,B1,B2]),
           (P "p" (C A), []),
           (P "p" (C B), []),
           (P "p" (C C_), []),
           (P "q" (C A), []),
           (P "q" (C B), []),
           (P "r" (V "X"), [P "p" (V "X"), P "q" (V "X")]),
           (PMult "s" [C A, C B], []),
           (PMult "t" [V "X", V "Y"], [P "p" (V "X"), PMult "s" [C A, V "Y"]]),  
           (PMult "v" [V "X", V "X"], [P "p" (V "X"), P "q" (V "X")])
           ]
{--
a0.
a1.
a2.
b0 :- a0, a1.
b1 :- a1, a2.
b2 :- a1, a2, d.
c0 :- b0, b1.
c1 :- b0, b1, b2.
d :- fail. % Actually, SWI-Prolog will otherwise complain that d/0 is not defined.
p(a).
p(b).
p(c).
q(a).
q(b).
r(X) :- p(X), q(X).
s(a,b).
t(X,Y) :- p(X), s(a,Y).
v(X,X) :- p(X), q(X).
--}
