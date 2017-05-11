{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

{- ===========================================================================
Contains basic types - you'll have to extend several of the definitions below
=========================================================================== -}


module FP_TypesEtc where

import GHC.Generics
import FPPrac.Trees

-- ===================================================================
-- Example Alphabet
-- - Extend, adapt, change the non-terminals to your own needs
-- - Do NOT change the first three groups of constructors (Symbol ... None)

data Alphabet = Terminal String               -- Terminal symbol: WILL be included in parseTree
              | Symbol   String               -- Terminal symbol: will NOT be included in parseTree
              | SyntCat  Alphabet             -- Checks whether a string belongs to a syntactic category

              -- EBNF constructions
              | Alt   [Alphabet] [Alphabet]   -- Try both
              | Opt   [Alphabet]              -- Optional
              | Rep0  [Alphabet]              -- Zero or more repetitions
              | Rep1  [Alphabet]              -- One or more repetitions

              | None                          -- For error messages
              -- Do NOT change the part above, this is generic and used by the parser generator

              -- The part below has to be adapted to your own needs
              | Program                       -- Start Symbol
              | Stmnt                         -- Statement
              | Block                         -- Block Statement
              | Expr                          -- Expression
              | SimExpr                       -- Simple Expression (i.e. a number or a variable)
              | ArExpr                        -- Arithmetic Expression
              | CompExpr                      -- Comparative Expression
              | Number                        -- Number
              | Variable                      -- Operation symbol
              | ArOp                          -- Arithmetic Operator
              | CompOp                        -- Comparative Operator
              | Space                         -- Spaces
              | Bracket                       -- Brackets
              | Brace                         -- Braces
              | AssignOp                      -- Assignment Operator
              | KW_Repeat                     -- Keyword (Repeat)
              | KW_EndRep                     -- Keyword (End Repeat)
              | KW_If                         -- Keyword (If)
              | KW_Else                       -- Keyword (Else)
              | KW_EndIf                      -- Keyword (End If)
              deriving (Eq,Ord,Show,Generic,ToRoseTree)

-- ===================================================================
-- Symbolic notation for EBNF constructors

ps <> qs = Alt  ps qs
(?:) ps  = Opt  ps
(*:) ps  = Rep0 ps
(+:) ps  = Rep1 ps

-- ===================================================================

type Grammar = Alphabet -> [[Alphabet]]

type Token   = (Alphabet,String)      -- Alphabet: indicates the "syntactic category" to which
                                      --      the String belongs (to distinguish, a.o., between
                                      --      reserved words and identifiers in general),
                                      -- String: the token itself,

instance ToRoseTree Token where
  toRoseTree t = RoseNode (show t) []

data ParseTree  = PLeaf Token
                | PNode Alphabet [ParseTree]
                | PError ParseTree [Alphabet] Alphabet String Int
                deriving (Eq,Show,Generic,ToRoseTree)

instance Ord ParseTree where
  PError _ _ _ _ k <  PError _ _ _ _ k' = k <  k'
  _                <  _                 = error "ordering only in case of parse-errors"

  PError _ _ _ _ k <= PError _ _ _ _ k' = k <= k'
  _                <= _                 = error "ordering only in case of parse-errors"

type ParseState = ( Alphabet       -- Non-terminal indicating the present subexpression
                  , [ParseTree]    -- The already produced trees within the present subexpression
                  , [(Int,Token)]  -- The remaining list of *indexed* input tokens
                  , [Alphabet]     -- List of non-terminals to check for left-recursiveness
                  )


-- ===================================================================
x ∈ xs = x `elem` xs

-- ===================================================================
-- Pretty Printing for Parse Trees
-- ===================================================================

toStrings ptree = case ptree of

     PLeaf t                 -> ["PLeaf " ++ show t]

     PNode nt ts             -> ("PNode " ++ show nt) : (addSpace 7 $ concat $ addEndBrack $ addListNotation $ map toStrings ts)
                             where
                               addSpace n = map ((replicate n ' ') ++)

                               addListNotation []                 =   [["["]]
                               addListNotation ([]:strss)         =   ["["]
                                                                    : [  (","++str'):strs' | (str':strs') <- strss ]
                               addListNotation ((str:strs):strss) =   (("["++str):strs)
                                                                    : [  (","++str'):strs' | (str':strs') <- strss ]

                               addEndBrack [strs]       = [ strs ++ ["]"] ]
                               addEndBrack (strs:strss) = strs : addEndBrack strss 

     PError tr rule nt str k -> [ "==========="
                                , "Parse Error"
                                , "==========="
                                , "Recognized:"
                                , "-----------"
                                ]
                                ++ toStrings tr ++
                                [ "-----------"
                                , "Still to go:   " ++ show rule
                                , "Expected:      " ++ show nt
                                , "Found:         " ++ str
                                , "At position:   " ++ show k
                                , "==========="
                                ]

prpr t  = putStr $ ('\n':) $ (++"\n") $ unlines $ toStrings t

