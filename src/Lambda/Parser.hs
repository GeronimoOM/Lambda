module Lambda.Parser where

import           Data.Functor.Identity (Identity)
import           Lambda.Core           (Expr (..), Name, (-->), (-->>), (<>))
import           Lambda.Expr           ((.>), (.>>), (.>>))
import qualified Lambda.Expr           as Ex
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Expr
import           Text.Parsec.String    (Parser)
import qualified Text.Parsec.Token     as Tok

lang :: Tok.LanguageDef ()
lang = Tok.LanguageDef {
    Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/"
  , Tok.commentLine     = "//"
  , Tok.nestedComments  = True
  , Tok.identStart      = lower
  , Tok.identLetter     = lower <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!&*+<=>|-?"
  , Tok.opLetter        = oneOf ":!&*+<=>|-?"
  , Tok.reservedNames   = ["if", "then", "else", "let", "rec", "in", "true", "false"]
  , Tok.reservedOpNames = ["\\", "->", "="]
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser lang

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

natural :: Parser Integer
natural = Tok.natural lexer

type Oper a = Operator String () Identity a
type OperTable a = OperatorTable String () Identity a

binary :: String -> (a -> a -> a) -> Assoc -> Oper a
binary s f = Infix (reservedOp s >> return f)

prefix :: String -> (a -> a) -> Oper a
prefix s f = Prefix (reservedOp s >> return f)

postfix :: String -> (a -> a) -> Oper a
postfix s f = Postfix (reservedOp s >> return f)

table :: OperTable Expr
table = [
  [binary "" (<>) AssocLeft],
  [binary "," (Ex.pair .>>) AssocLeft],
  [postfix "++" (Ex.succ .>), postfix "--" (Ex.pred .>),
   prefix "!" (Ex.not .>), postfix "?" (Ex.iszero .>)],
  [binary "*" (Ex.mult .>>) AssocNone],
  [binary "+" (Ex.add .>>) AssocNone],
  [binary ">=" (Ex.gte .>>) AssocLeft, binary "==" (Ex.eq .>>) AssocLeft,
    binary ">" (Ex.gt .>>) AssocLeft, binary "<=" (Ex.lte .>>) AssocLeft,
    binary "<" (Ex.lt .>>) AssocLeft, binary "/=" (Ex.neq .>>) AssocLeft],
  [binary "&" (Ex.and .>>) AssocLeft],
  [binary "|" (Ex.or .>>) AssocLeft]
 ]

ifThenElse :: Parser Expr
ifThenElse = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (Ex.ifThenElse <> cond <> tr <> fl)

lam :: Parser Expr
lam = do
  reservedOp "\\"
  vs <- sepBy1 identifier spaces
  reservedOp "->"
  e <- expr
  return $ vs -->> e

var :: Parser Expr
var = do
  n <- identifier
  return $ Var n

true :: Parser Expr
true = do
  reserved "true"
  return Ex.true

false :: Parser Expr
false = do
  reserved "false"
  return Ex.false

num :: Parser Expr
num = do
  n <- natural
  return (Ex.num (fromInteger n))

literal :: Parser Expr
literal = true <|> false <|> num

letBody :: Parser (Name, Expr, Expr)
letBody = do
  v <- identifier
  ps <- many identifier
  reservedOp "="
  t <- expr
  reserved "in"
  e <- expr
  return (v, ps -->> t, e)

letIn :: Parser Expr
letIn = do
  reserved "let"
  (v, t, e) <- letBody
  return $ Ex.letIn v t e

letRecIn :: Parser Expr
letRecIn = do
  reserved "let"
  reserved "rec"
  (v, t, e) <- letBody
  return $ Ex.letRecIn v t e

term :: Parser Expr
term = choice [
  parens expr,
  ifThenElse,
  try letRecIn,
  try letIn,
  lam,
  literal,
  var
 ]

expr :: Parser Expr
expr = buildExpressionParser table term

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""
