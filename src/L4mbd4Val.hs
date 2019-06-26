module L4mbd4Val where

import Data.List

data L4mbd4Val = Variable String
               | Id String
               | List [L4mbd4Val]
               | Def String L4mbd4Val
               | Lambda String L4mbd4Val
--               | Brackets L4mbd4Val
               | Error String
               deriving (Eq)

instance Show L4mbd4Val where
  show (Variable name) = name
  show (Id name) = name
  show (Def name body) = name ++ ":=" ++ (show body)
  show (Lambda name body) = "\\" ++ name ++ "." ++ (show body)
--  show (Brackets val) = "(" ++ (show val) ++ ")"
  show (Error message) = "Error : " ++ message
  show (List x) = "(" ++ (intercalate " " $ map show x) ++ ")"
