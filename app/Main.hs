module Main where

import Control.Monad (liftM)
import Control.Monad.Except
import Evaluator (eval, primitiveBindings)
import Parser (readExpr)
import System.Environment (getArgs)
import System.IO
import Types

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runOne args

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings
  env <- bindVars env [("args", List $ map String $ drop 1 args)]
  val <- runIOThrows $ show <$> eval env (List [Atom "load", String (head args)])
  hPutStrLn stderr val

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn