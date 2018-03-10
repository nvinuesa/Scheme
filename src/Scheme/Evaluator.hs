module Scheme.Evaluator
  ( eval
  ) where

import Scheme.Data

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func:args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("symbol?", unaryOp symbolTest)
  , ("string?", unaryOp stringTest)
  , ("number?", unaryOp numberTest)
  , ("bool?", unaryOp boolTest)
  , ("list?", unaryOp listTest)
  , ("symbol->string", unaryOp symbol2string)
  , ("string->symbol", unaryOp string2symbol)
  ]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

symbolTest, stringTest, numberTest, boolTest, symbol2string, string2symbol ::
     LispVal -> LispVal
symbolTest (Atom _) = Bool True
symbolTest _ = Bool False

stringTest (String _) = Bool True
stringTest _ = Bool False

numberTest (Number _) = Bool True
numberTest _ = Bool False

boolTest (Bool _) = Bool True
boolTest _ = Bool False

listTest (List _) = Bool True
listTest (DottedList _ _) = Bool True
listTest _ = Bool False

symbol2string (Atom s) = String s
symbol2string _ = String ""

string2symbol (String s) = Atom s
string2symbol _ = Atom ""

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0
