module Lambda.Shell where

import           Control.Monad             (unless)
import           Control.Monad.State
import           Data.Map                  as M
import           Data.Maybe                (fromJust, fromMaybe)
import           Lambda.Core
import qualified Lambda.Efficient.Adapter  as Ad
import qualified Lambda.Efficient.Nameless as Ef
import           Lambda.Eval
import           Lambda.Parser
import           System.Console.Haskeline
import           Text.Parsec
import           Text.Parsec.Char          (char)
import           Text.Parsec.String        (Parser)


type Shell a = InputT IO a

run :: IO ()
run = runInputT defaultSettings loop

loop :: Shell ()
loop = do
  input <- inp
  case parse cmndInput "" input of
    Left err -> out err >> loop
    Right (mc, input) -> do
      cont <- case fromMaybe defCmnd mc of
        Quit -> return False
        Eval -> onEval input
        List -> onList input
        Eff  -> onEff input
      when cont loop

onEval :: String -> Shell Bool
onEval input = do
  onParseExpr input (outValMaybe . eval)
  return True

onList :: String -> Shell Bool
onList input = do
  onParseExpr input (outLs . evalLs)
  return True where
    outLs ls = do
      mapM_  outNumExpr (zip [1..] ls)
      outValMaybe (last ls)
    outNumExpr (n, e) = outS ( "[" ++ show n ++ "] " ++ show e)

onEff :: String -> Shell Bool
onEff input = do
  onParseExpr input outEff
  return True where
    outEff e = maybe (out "Expression does not evaluate to a valid value")
      out (Ad.effEvalToValue e)

pref :: String
pref = "[λ] "

out :: Show s => s -> Shell ()
out s = outputStrLn (pref ++ show s)

outS :: String -> Shell ()
outS s = outputStrLn (pref ++ s)

inp :: Shell String
inp = fmap fromJust (getInputLine pref)

data Command = Quit | Eval | List | Eff

commands :: Map String Command
commands = insert "eval" Eval .
           insert "list" List .
           insert "eff" Eff .
           insert "quit" Quit $
           empty

toCmnd :: String -> Maybe Command
toCmnd s = M.lookup s commands

defCmnd :: Command
defCmnd = Eval

cmnd :: Parser Command
cmnd = do
  char ':'
  s <- many1 lower
  maybe (parserFail "Invalid command") return (toCmnd s)

cmndInput :: Parser (Maybe Command, String)
cmndInput = do
  mc <- optionMaybe cmnd
  spaces
  input <- getInput
  return (mc, input)

outValMaybe :: Expr -> Shell ()
outValMaybe e = maybe (out e) out (toValue e)

onParseExpr :: String -> (Expr -> Shell ()) -> Shell ()
onParseExpr input f = either out f (parse expr "" input)
