module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data L4mbd4Val = Id String
               | Variable String
               | Expression L4mbd4Val
               | Lambda  L4mbd4Val L4mbd4Val
               | Def  L4mbd4Val L4mbd4Val
               | List [L4mbd4Val]
               | Program L4mbd4Val L4mbd4Val
               deriving (Show)

spaces :: Parser ()
spaces = skipMany1 space

parseId :: Parser L4mbd4Val
parseId = do
  name <- many1 upper
  return $ Id name

parseVariable :: Parser L4mbd4Val
parseVariable = do
  name <- many1 lower
  return $ Variable name

parseLambda :: Parser L4mbd4Val
parseLambda = do
  char '\\'
  v <- parseVariable
  char '.'
  body <- parseExpr
  return $ Lambda v body

parseDef :: Parser L4mbd4Val
parseDef = do
  name <- parseId
  char ':'
  char '='
  body <- parseLambda
  return $ Def name body

parseDefList :: Parser L4mbd4Val
parseDefList = do
  x <- sepBy parseDef (char ';')
  return $ List x

parseProgram :: Parser L4mbd4Val
parseProgram = do
  defs <- parseDefList
  char '?'
  expr <- parseExpr
  return $ Program defs expr

parseExpr :: Parser L4mbd4Val
parseExpr = parseLambda
         <|> parseId
         <|> parseVariable

readExpr :: String -> String
readExpr input = case parse (parseProgram <|> parseExpr) "l4mbd4" input of
   Left err -> "No match: " ++ show err
   Right val -> "Found value: " ++ show val
