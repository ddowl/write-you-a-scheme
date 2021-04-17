module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  putStrLn "Enter name: "
  name <- getLine
  putStrLn ("Hello, " ++ name)