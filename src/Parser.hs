module Parser
  ( symbol,
    readExpr,
    readExprList,
  )
where

import Control.Monad (liftM)
import Control.Monad.Except
import Numeric (readHex, readOct)
import System.Environment ()
import Text.ParserCombinators.Parsec hiding (spaces)
import Types

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (parseEscapeChar <|> noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseInteger <|> parseHex <|> parseOct

parseInteger :: Parser LispVal
parseInteger = Number . read <$> many1 digit

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  h <- many1 octDigit
  let octValue = (fst . head) $ readOct h
  return $ Number octValue

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  h <- many1 hexDigit
  let hexValue = (fst . head) $ readHex h
  return $ Number hexValue

parseEscapeChar :: Parser Char
parseEscapeChar = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    '\\' -> x
    '"' -> x
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _ -> x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space