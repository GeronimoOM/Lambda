module Lambda.Console.Command where

import           Data.Map           as M (Map, empty, insert, lookup)
import           Text.Parsec
import           Text.Parsec.String (Parser)

data Command = Eval | List | Eff | Add | Ctx | Quit | Mogo
  deriving (Show, Eq)

commands :: Map String Command
commands = insert "eval" Eval .
           insert "list" List .
           insert "eff"  Eff .
           insert "add"  Add .
           insert "ctx"  Ctx .
           insert "mogo" Mogo .
           insert "quit" Quit $
           empty

toCmd :: String -> Maybe Command
toCmd s = M.lookup s commands

cmd :: Parser Command
cmd = do
  char ':'
  s <- many1 lower
  maybe (parserFail "invalid command") return (toCmd s)

cmdInput :: Parser (Maybe Command, String)
cmdInput = do
  mc <- optionMaybe cmd
  spaces
  input <- getInput
  return (mc, input)
