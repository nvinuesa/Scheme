module Scheme.Data
  ( LispVal(..)
  , showVal
  ) where

import Data.Array
import Data.Complex (Complex(..))

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | Ratio Rational
  | Float Double
  | Complex (Complex Double)
  | String String
  | Char Char
  | Bool Bool
  | Vector (Array Int LispVal)

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

--
-- Helpers
--
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
