module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data L4mbd4Val = Variable String
               | Id String
               | List [L4mbd4Val]
               | Def L4mbd4Val L4mbd4Val
               | Lambda L4mbd4Val L4mbd4Val
               | Brackets L4mbd4Val
               | Error String
               deriving (Eq,Show)

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
  name <- parseId
  char ':'
  char '='
  body <- parseExprList
  return $ Def name body

parseLambda :: Parser L4mbd4Val
parseLambda = do
  char '\\'
  v <- parseVariable
  char '.'
  body <- parseExprList
  return $ Lambda v body

parseExpr :: Parser L4mbd4Val
parseExpr = parseVariable
         <|> parseDef
         <|> parseLambda
         <|> do char '('
                x <- parseExprList
                char ')'
                return $ Brackets x

parseExprList :: Parser L4mbd4Val
parseExprList = do
  spaces
  x <- sepEndBy parseExpr spaces
  spaces
  return $ List x

readExpr :: String -> String
readExpr input = case parse parseExprList "l4mbd4fun" input of
   Left err -> "No match: " ++ show err
   Right val -> show val
