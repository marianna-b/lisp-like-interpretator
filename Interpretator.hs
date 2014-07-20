import LispParser
import LispEval

main :: IO ()
main = do
       a <- getLine
       str <- readFile a
       print $ toString $ evalLisp $ lexer str
