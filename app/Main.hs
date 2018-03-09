module Main where

import Scheme
import Scheme.Evaluator
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
