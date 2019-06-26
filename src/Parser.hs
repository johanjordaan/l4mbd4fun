module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

import L4mbd4Val

spaces :: Parser ()
spaces = skipMany space

parseId :: Parser L4mbd4Val
parseId = do
  name <- many1 upper
  return $ Id name

parseVariable :: Parser L4mbd4Val
parseVariable = do
  name <- many1 lower
  return $ Variable name

parseDef :: Parser L4mbd4Val
parseDef = do
  Id name <- parseId
  spaces
  char ':'
  char '='
  spaces
  body <- parseExprList
  return $ Def name body

parseLambda :: Parser L4mbd4Val
parseLambda = do
  char '\\'
  Variable name <- parseVariable
  char '.'
  body <- parseExprList
  return $ Lambda name body

parseExpr :: Parser L4mbd4Val
parseExpr = parseVariable
         <|> (try parseDef <|> parseId)
         <|> parseLambda
         <|> do char '('
                x <- parseExprList
                char ')'
                return $ x

parseExprList :: Parser L4mbd4Val
parseExprList = do
  spaces
  x <- sepEndBy parseExpr spaces
  spaces
  return $ List x

readExpr :: String -> L4mbd4Val
readExpr input = case parse parseExprList "l4mbd4fun" input of
   Left err -> Error $ show err
   Right val -> val
