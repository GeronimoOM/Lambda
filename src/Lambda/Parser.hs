module Lambda.Parser
  (ifThenElse, lam, name, var, true, false,
    num, literal, def, expr)
where

import           Data.Functor.Identity (Identity)
import           Data.Maybe            (isNothing)
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
  , Tok.opStart         = oneOf ":!&*+<=>|-?/%"
  , Tok.opLetter        = oneOf ":!&*+<=>|-?/%"
  , Tok.reservedNames   = ["if", "then", "else", "let", "rec", "in",
  "true", "false", "fst", "snd", "div"]
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

binaryName :: String -> (a -> a -> a) -> Assoc -> Oper a
binaryName s f = Infix (reserved s >> return f)

prefix :: String -> (a -> a) -> Oper a
prefix s f = Prefix (reservedOp s >> return f)

postfix :: String -> (a -> a) -> Oper a
postfix s f = Postfix (reservedOp s >> return f)

table :: OperTable Expr
table = [
  [postfix "++" (Ex.succ .>), postfix "--" (Ex.pred .>),
   prefix "!" (Ex.not .>), postfix "?" (Ex.isZero .>)],
  [binary "," (Ex.pair .>>) AssocRight],
  [binary "" (<>) AssocLeft],
  [binary "*" (Ex.mult .>>) AssocLeft, binary "/" (Ex.quot .>>) AssocLeft,
  binary "%" (Ex.rem  .>>) AssocLeft, binaryName "div" (Ex.div .>>) AssocLeft],
  [binary "+" (Ex.add .>>) AssocLeft, binary "-" (Ex.sub .>>) AssocLeft],
  [binary ">=" (Ex.gte .>>) AssocNone, binary "==" (Ex.eq .>>) AssocNone,
    binary ">" (Ex.gt .>>) AssocNone, binary "<=" (Ex.lte .>>) AssocNone,
    binary "<" (Ex.lt .>>) AssocNone, binary "/=" (Ex.neq .>>) AssocNone],
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

name :: Parser Name
name = identifier

var :: Parser Expr
var = do
  n <- name
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

first :: Parser Expr
first = do
  reserved "fst"
  return Ex.fst

second :: Parser Expr
second = do
  reserved "snd"
  return Ex.snd

oper :: Parser Expr
oper = first <|> second

def :: Parser (Name, Expr)
def = do
  r <- optionMaybe (reserved "rec")
  n <- name
  ps <- many name
  reservedOp "="
  e <- expr
  return (n, (if isNothing r then id else Ex.recurs n) (ps -->> e))

letIn :: Parser Expr
letIn = do
  reserved "let"
  (n, t) <- def
  reserved "in"
  e <- expr
  return $ Ex.letIn n t e

term :: Parser Expr
term = choice [
  parens expr,
  ifThenElse,
  letIn,
  lam,
  oper,
  literal,
  var
 ]

expr :: Parser Expr
expr = buildExpressionParser table term
