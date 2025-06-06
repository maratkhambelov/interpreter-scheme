module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Ratio
import Data.Complex
import Numeric (readHex, readOct, readFloat)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
  | Float Double
  | Ratio Rational
  | Complex (Complex Double)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    '\\' -> x
    '"'  -> x
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space")
        <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
  return $ Character $ case value of
    "space"   -> ' '
    "newline" -> '\n'
    _         -> value !! 0

parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return $ Ratio (read x % read y)

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList  :: Parser LispVal
parseDottedList = do 
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float (fst . head $ readFloat (x ++ "." ++ y))

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= return . Number . read

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
  try $ string "#d"
  x <- many1 digit
  return . Number . read $ x

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  x <- many1 octDigit
  return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  x <- many1 (oneOf "10")
  return $ Number (bin2dig x)

oct2dig :: String -> Integer
oct2dig x = fst $ head $ readOct x

hex2dig :: String -> Integer
hex2dig x = fst $ head $ readHex x

bin2dig :: String -> Integer
bin2dig = bin2dig' 0

bin2dig' :: Integer -> String -> Integer
bin2dig' digint ""     = digint
bin2dig' digint (x:xs) = bin2dig' (2 * digint + if x == '0' then 0 else 1) xs

toDouble :: LispVal -> Double
toDouble (Float f)  = realToFrac f
toDouble (Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseDecimal1
  char '+'
  y <- try parseFloat <|> parseDecimal1
  char 'i'
  return $ Complex (toDouble x :+ toDouble y)

parseExpr :: Parser LispVal
parseExpr =
      try parseComplex
  <|> try parseFloat
  <|> try parseRatio
  <|> try parseNumber
  <|> try parseBool
  <|> try parseCharacter
  <|> (do char '('
          x <- try parseList <|> parseDottedList
          char ')'
          return x)
  <|> parseString
  <|> parseAtom

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr (args !! 0)



