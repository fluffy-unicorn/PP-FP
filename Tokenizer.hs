module Tokenizer where

import FP_TypesEtc
import Data.List

data State = Init | Num1 | Num2 | Num3 | Idf1 | OpsPlus | OpsDiv | OpsEq | OpsSub | OpsNEq | OpsAcc | Single | WS1 | Error deriving Show

isDigit :: Char -> Bool
isDigit x = x `elem` ['0'..'9']

isAlpha :: Char -> Bool
isAlpha x = x `elem` ['A'..'z'] \\ ['['..'`']

fsaNum :: State -> Char -> State
fsaNum Init x | isDigit x || x == '~' = Num1
fsaNum Num1 x | isDigit x = Num1
              | x == '.'  = Num2
fsaNum Num2 x | isDigit x = Num2
fsaNum _    _ = Error

fsaIdf :: State -> Char -> State 
fsaIdf Init x | isAlpha x = Idf1
fsaIdf Idf1 x | isDigit x || isAlpha x = Idf1
fsaIdf _    _ = Error

fsaOps :: State -> Char -> State
fsaOps Init    x | x `elem` "*^%" = OpsAcc
                 | x `elem` "<>"  = OpsNEq
                 | x == '+' = OpsPlus
                 | x == '-' = OpsSub
                 | x == '/' = OpsDiv
                 | x == '=' = OpsEq
fsaOps OpsPlus x | x == '+' = OpsAcc
fsaOps OpsSub  x | x == '-' = OpsAcc
fsaOps OpsDiv  x | x == '/' = OpsAcc
fsaOps OpsEq   x | x == '=' = OpsAcc
fsaOps OpsNEq  x | x == '=' = OpsAcc
fsaOps _       _ = Error

fsaSingle :: State -> Char -> State
fsaSingle Init '(' = Single
fsaSingle Init ')' = Single
fsaSingle Init '{' = Single
fsaSingle Init '}' = Single
fsaSingle _    _   = Error

fsaWS :: State -> Char -> State
fsaWS Init x | x == ' ' = WS1
fsaWS WS1  x | x == ' ' = WS1
fsaWS _    _ = Error

getFsa :: Char -> (State -> Char -> State)
getFsa x | isDigit x ||x == '~' = fsaNum
         | isAlpha x            = fsaIdf
         | x `elem` "*^%<>+-/=" = fsaOps
         | x `elem` "(){}"      = fsaSingle
         | x == ' '             = fsaWS
         | otherwise = (\_ _ -> Error)

tokenizer :: String -> [(Alphabet, String)]
tokenizer xs = map lexer (allTokens xs [])

allTokens :: String -> [String] -> [String]
allTokens "" lst = lst
allTokens (x:xs) lst | result == [] = allTokens rest lst
                     | otherwise = allTokens rest (lst ++ [result])
                       where (result, rest) = nextToken (getFsa x) Init "" (x:xs)

nextToken :: (State -> Char -> State) -> State -> String -> String -> (String, String)
nextToken fsa s t "" = (t, "")
nextToken fsa s t (x:xs) = case result of
                            Error -> (t, (x:xs))
                            WS1   -> ([], xs)
                            _     -> nextToken fsa result (t ++ [x]) xs
                           where 
                            result = fsa s x

lexer :: String -> (Alphabet, String)
lexer xs | xs == "loop" = (KW_Repeat, xs)
         | xs == "pool" = (KW_EndRep, xs)
         | xs == "if"   = (KW_If, xs)
         | xs == "else" = (KW_Else, xs)
         | xs == "fi"   = (KW_EndIf, xs)
         | (isDigit $ head xs) || head xs == '~' = (Number, xs)
         | xs == "=" = (AssignOp, xs)
         | (head xs) `elem` "*^%+-/" = (ArOp, xs)
         | (head xs) `elem` "<>=" = (CompOp, xs)
         | xs == "(" || xs == ")" = (Bracket, xs)
         | xs == "{" || xs == "}" = (Brace, xs)
         | otherwise = (Variable, xs)
             
