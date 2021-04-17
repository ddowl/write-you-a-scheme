module Main where

import Evaluator (eval)
import Parser (readExpr)
import System.Environment (getArgs)

main :: IO ()
main = do
  getArgs >>= print . eval . readExpr . head
