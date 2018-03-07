module Scheme.Data
  ( LispVal(..)
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
  deriving (Show)
