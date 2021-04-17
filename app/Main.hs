module Main where

import Control.Monad (liftM)
import Errors (extractValue, trapError)
import Evaluator (eval)
import Parser (readExpr)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
