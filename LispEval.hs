module LispEval where

import Functions
import MonadicParser


evalLisp :: [Lexem] -> Either Error [Lexem]
evalLisp [] = Right []
evalLisp ((List x):as) =  unEval (evalList x) (evalLisp as)
evalLisp (a:as) = unEval (Right [a]) (evalLisp as)


toString :: Either Error [Lexem] -> String
toString a =
  case a of
    Left s -> s
    Right [] -> ""
    Right (x:xs) ->
      case x of
        List l -> ['('] ++ (toString (Right l)) ++ [')'] ++ " " ++ (toString (Right xs))
        Name l -> l ++ " " ++ (toString (Right xs))
        Str l -> ['"'] ++ l ++ ['"'] ++ " " ++ (toString (Right xs))
        Number l -> (show l) ++ " " ++ (toString (Right xs))
