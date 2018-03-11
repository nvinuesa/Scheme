module Main where

import Control.Monad (liftM)
import Scheme
import Scheme.Data
import Scheme.Evaluator
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
