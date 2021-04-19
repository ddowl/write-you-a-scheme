{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Evaluator where

import Control.Monad.Except
import Data.Functor ((<&>))
import Data.IORef
import Data.Maybe (isJust, isNothing)
import Debug.Trace (trace)
import Parser (readExpr, readExprList)
import System.IO (IOMode (ReadMode, WriteMode), hClose, hGetLine, hPrint, openFile, stdin, stdout)
import Types

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool True -> eval env conseq
    Bool False -> eval env alt
    r -> throwError $ TypeMismatch "predicates must return boolean" r
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List (Atom "cond" : clauses)) = cond env clauses
eval env (List (Atom "case" : key : clauses)) = caseE env key clauses
eval env (List [Atom "load", String filename]) =
  load filename >>= fmap last . mapM (eval env)
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args =
  if length params /= length args && isNothing varargs
    then throwError $ NumArgs (toInteger $ length params) args
    else do
      let fnArgs = zip params args
      fnEnv <- liftIO $ bindVars closure fnArgs
      fnEnv <- case varargs of
        Just argName ->
          let remainingArgs = drop (length params) args
           in liftIO $ bindVars fnEnv [(argName, List remainingArgs)]
        Nothing -> return fnEnv
      last <$> mapM (eval fnEnv) body

primitiveBindings :: IO Env
primitiveBindings = do
  env <- nullEnv
  let primitiveFuncs = map (makeFunc PrimitiveFunc) primitives
  let ioFuncs = map (makeFunc IOFunc) ioPrimitives
  bindVars env (primitiveFuncs ++ ioFuncs)
  where
    makeFunc constructor (var, func) = (var, constructor func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)
  ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)
  ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftIO $ do
  handle <- openFile filename mode
  return $ Port handle

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = do
  contents <- liftIO $ readFile filename
  return $ String contents

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

numericBinop ::
  (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

boolBinop ::
  (LispVal -> ThrowsError a) ->
  (a -> a -> Bool) ->
  [LispVal] ->
  ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ args !! 0
      right <- unpacker $ args !! 1
      return $ Bool (left `op` right)

numBoolBinop = boolBinop unpackNum

strBoolBinop = boolBinop unpackStr

boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return $ show n
unpackStr (Bool b) = return $ show b
unpackStr notStr = throwError $ TypeMismatch "not string" notStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else (return . fst . head) parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs last] = return $ DottedList (x : xs) last
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

-- https://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_idx_106
cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond env [] = throwError $ Default "No valid cond clauses"
cond env [List (Atom "else" : expressions)] = lastExpr env expressions
cond env (clause : otherClauses) = case clause of
  (List [test]) -> eval env test
  val@(List [test, Atom "=>", expr]) -> throwError $ TypeMismatch "Function values aren't supported in cond" val
  (List (test : exprs)) -> do
    result <- eval env test
    case result of
      Bool True -> lastExpr env exprs
      Bool False -> cond env otherClauses
      r -> throwError $ TypeMismatch "predicates must return boolean" r
  badClause -> throwError $ TypeMismatch "cond clause must contain a test" badClause

-- https://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_idx_114
caseE :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
caseE env key [] = throwError $ Default "No matching case clauses"
caseE env key (List (Atom "else" : exprs) : otherClauses) = lastExpr env exprs
caseE env key (List (List datums : exprs) : otherClauses) = do
  keyValue <- eval env key
  eqvs <- liftThrows $ mapM (\d -> eqv [keyValue, d]) datums
  if Bool True `elem` eqvs
    then lastExpr env exprs
    else caseE env key otherClauses
caseE env key x = throwError $ TypeMismatch "well formed case clause" (List x)

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv
  [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] =
  return $ Bool $ length arg1 == length arg2 && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
      Left err -> False
      Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
    `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <-
    or
      <$> mapM
        (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

lastExpr :: Env -> [LispVal] -> IOThrowsError LispVal
lastExpr env exprs = last (map (eval env) exprs)
