module Main where

import System.Environment
import Data.List
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ "parsing " ++ expr
  putStrLn $ readExpr expr

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Float Float
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (escapedChars <|> noneOf "\"" )
                char '"'
                return $ String x

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf ['"', 'n', 't']
  return $ case x of
             'n' -> '\n'
             't' -> '\t'
             _ -> x


parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseFloat :: Parser LispVal
parseFloat = do
  int <- many1 digit
  char '.'
  dec <- many1 digit
  return $ (Float . read) (int ++ "." ++ dec)

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
parseExpr = parseAtom
         <|> parseString
         <|> parseFloat
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x





