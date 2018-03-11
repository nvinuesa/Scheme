module Scheme.Evaluator
  ( eval
  ) where

import Control.Monad.Error
import Scheme.Data

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func:args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v

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

numericBinop ::
     (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
  in if null parsed
       then throwError $ TypeMismatch "number" $ String n
       else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
