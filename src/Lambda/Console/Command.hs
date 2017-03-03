module Lambda.Console.Command where

import           Data.Map.Strict    as M (Map, empty, insert, lookup)
import           Text.Parsec
import           Text.Parsec.String (Parser)

data Command = Eval | List | Steps | Eff
  | Set | Load | Ctx | Rmv | Clr | Quit | Test
  deriving (Show, Eq)

commands :: Map String Command
commands = insert "eval"  Eval .
           insert "list"  List .
           insert "steps" Steps .
           insert "eff"   Eff .
           insert "set"   Set .
           insert "load"  Load .
           insert "ctx"   Ctx .
           insert "rmv"   Rmv .
           insert "clr"   Clr .
           insert "test"  Test .
           insert "quit"  Quit $
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
