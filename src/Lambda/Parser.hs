module Lambda.Parser where

import           Lambda
import           Text.Parsec
import           Text.Parsec.String

appl :: Parser Expr
appl = chainl1 term applOp

applOp :: Parser (Expr -> Expr -> Expr)
applOp = do { spaces; return (-$-) }

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

term :: Parser Expr
term = parens term <|> func <|> appl <|> var

func :: Parser Expr
func = do
  char 'L'
  vars <- sepBy1 name spaces
  char '.'
  expr <- appl
  return $ vars -->> expr

name :: Parser Var
name = lower

var :: Parser Expr
var = do { v <- name; return $ Var v }

expr :: Parser Expr
expr = appl

parseExpr :: String -> Either ParseError Expr
parseExpr = parse appl ""
