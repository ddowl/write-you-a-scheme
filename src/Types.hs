module Types where

import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), MonadIO (liftIO))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Text.Parsec

-- AST

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func
      { params :: [String],
        vararg :: Maybe String,
        body :: [LispVal],
        closure :: Env
      }

instance Eq LispVal where (==) = eqVal

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs, body = body, closure = env} =
  "(lambda (" ++ unwords (map show args)
    ++ ( case varargs of
           Nothing -> ""
           Just arg -> " . " ++ arg
       )
    ++ ") ...)"

eqVal :: LispVal -> LispVal -> Bool
eqVal (String a) (String b) = a == b
eqVal (Atom a) (Atom b) = a == b
eqVal (Number a) (Number b) = a == b
eqVal (Bool a) (Bool b) = a == b
eqVal (List a) (List b) = a == b
eqVal (DottedList aHead aTail) (DottedList bHead bTail) = aHead == bHead && aTail == bTail
eqVal _ _ = False

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarArgs = makeFunc . Just . showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- ENV

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

-- ERRORS

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default err) = show err

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
