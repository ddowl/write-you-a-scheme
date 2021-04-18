module Env where

import Control.Monad.Except (liftIO, throwError)
import Data.IORef
import Data.Maybe (isJust)
import Errors
import Types

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = do
  env <- readIORef envRef
  let maybeValRef = lookup var env
  return $ isJust maybeValRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  let maybeValRef = lookup var env
  case maybeValRef of
    Just valRef -> liftIO $ readIORef valRef
    Nothing -> throwError $ UnboundVar "Getting an unbound variable" var

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  let maybeValRef = lookup var env
  case maybeValRef of
    Just valRef -> liftIO $ writeIORef valRef value
    Nothing -> throwError $ UnboundVar "Setting an unbound variable" var
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
  env <- readIORef envRef
  newEnv <- fmap (++ env) (mapM addBindingRef bindings)
  newIORef newEnv
  where
    addBindingRef (var, value) = newIORef value >>= (\ref -> return (var, ref))