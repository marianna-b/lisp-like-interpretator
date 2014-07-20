module Functions where

import MonadicParser
import Data.List

functionNames :: [String]
functionNames = ["eval", "quote", "car", "cdr", "cons"]

type Function = [Lexem] -> Either Error [Lexem]

functions :: [Function]
functions = [eval', quote', car', cdr', cons']

eval' :: Function
eval' [] = Right []
eval' (a:as) =
  case a of
    List x -> case (eval' x) of
                Left str -> Left str
                Right ex -> unEval (evalList ex) (eval' as)
    _      -> unEval (Right [a]) (eval' as)

quote' :: Function
quote' a = case a of
             (x:[]) -> Right [x]
             _      -> Right $ [List a]

car' :: Function
car' ((List x):[]) =
  case x of
    [] -> Left "empty list"
    (a:_) -> Right [a]
car' _ = Left "incorrect arg car"


cdr' :: Function
cdr' ((List x):[]) =
  case x of
    [] -> Left "empty list"
    (_:as) -> Right $ [List as]
cdr' _ = Left "incorrect arg cdr"

cons' :: Function
cons' ((List a):((List x):[])) = Right $ [List $ a ++ x]
cons' _ = Left "incorrect arg cons"

--Evaluation stuf---------------------------------------------------------------

unEval :: Either Error [Lexem] -> Either Error [Lexem] -> Either Error [Lexem]
unEval a b = case a of Left e -> Left e
                       Right x -> case b of Left eb -> Left eb
                                            Right y -> Right (x ++ y)


evalList :: [Lexem] -> Either Error [Lexem]
evalList [] = Right $ []
evalList ((Name x):as) = evalFunc x as
evalList aas = Right $ aas


evalFunc :: String -> [Lexem] -> Either Error [Lexem]
evalFunc name args = if isFun name then
                           case (getFun name) args of
                                Left x -> Left x
                                Right s -> Right s
                     else
                       Left "unknown function"

isFun :: String -> Bool
isFun name = name `elem` functionNames

getFun :: String -> Function
getFun name = functions!!(getFunIdx name)

getFunIdx :: String -> Int
getFunIdx name = case (elemIndex name functionNames) of
                   Just a -> a
