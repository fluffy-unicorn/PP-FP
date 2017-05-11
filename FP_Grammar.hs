{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}
        -- Necessary for function toRoseTree

module FP_Grammar where

{- ===========================================================================
Contains example grammar + examples of test definitions
NOTE: Compiler directives above
=========================================================================== -}

import FPPrac.Trees             -- Contains now also the function toRoseTree. Re-install it!
import GHC.Generics             -- Necessary for correct function of FPPrac

import FP_TypesEtc              -- Extend the file TypesEtc with your own alphabet
import FP_ParserGen (parse)     -- Touching this file leaves you at your own devices

-- import Tokenizer             -- You'll have to write a file for tokenizing yourself

-- ==========================================================================================================
-- Example grammar, to illustrate the structure of the definition of the grammar as a function
--      (where the Alphabet is in the file TypesEtc.hs)

grammar :: Grammar

grammar nt = case nt of

        Expr    -> [[ lBracket, Expr, Op, Expr, rBracket ]
                   ,[ Nmbr                               ]]

        Nmbr    -> [[ nmbr                               ]]

        Op      -> [[ op                                 ]]


-- shorthand names can be handy, such as:
lBracket  = Terminal "("           -- Terminals WILL be shown in the parse tree
rBracket  = Terminal ")"

-- alternative for brackets (try both variants and compare the parse trees):
-- lBracket  = Symbol "("          -- Symbols will NOT be shown in the parse tree.
-- rBracket  = Symbol ")"

nmbr      = SyntCat Nmbr
op        = SyntCat Op



-- ==========================================================================================================
-- TESTING: example expression: "((10+20)*30)"
-- ==========================================================================================================

-- Result of tokenizer (to write yourself) should be a tokenList like here.
-- NOTE: a lexer is needed (i.e., a function that maps a String to a tuple (Alphabet,String).
--                         (opens e.g. the possibility to differentiate between variables and keywords)
tokenList = [ (Bracket, "(" )
            , (Bracket, "(" )
            , (Nmbr   , "10")
            , (Op     , "+" )
            , (Nmbr   , "20")
            , (Bracket, ")" )
            , (Op     , "*" )
            , (Nmbr   , "30")
            , (Bracket, ")" )
            ]

-- Parse this token list with a call to the function parse, with
--      - grammar: the name of the grammar above
--      - Expr: the start-nonterminal of the grammar above
--      - tokenList: the token list above
parseTree0 = parse grammar Expr tokenList

-- prpr: for pretty-printing the parsetree, including error messages
testTxt    = prpr parseTree0

-- showTree + toRoseTree: for graphical representation in browser
testGr     = showRoseTree $ toRoseTree parseTree0


