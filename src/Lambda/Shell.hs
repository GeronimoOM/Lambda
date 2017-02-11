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
  unless (input == quit) $ do
    case parse (spaces >> cmndExpr) "" input of
      (Left err) -> out err
      (Right (mc, e)) ->
        case fromMaybe defCmnd mc of
          Eval -> onEval e
          List -> onList e
          Eff  -> onEff e
    loop

pref :: String
pref = "[λ] "

quit :: String
quit = ":quit"

out :: Show s => s -> Shell ()
out s = outputStrLn (pref ++ show s)

outS :: String -> Shell ()
outS = outputStrLn

inp :: Shell String
inp = fmap fromJust (getInputLine pref)

data Command = Eval | List | Eff

commands :: Map String Command
commands = insert "eval" Eval .
           insert "list" List .
           insert "eff" Eff $
           empty

toCmnd :: String -> Maybe Command
toCmnd s = M.lookup s commands

defCmnd :: Command
defCmnd = Eval

cmnd :: Parser Command
cmnd = do
  char ':'
  s <- many1 lower
  spaces
  case toCmnd s of
    Just c  -> return c
    Nothing -> parserFail "Invalid command"

cmndExpr :: Parser (Maybe Command, Expr)
cmndExpr = do
  mc <- optionMaybe cmnd
  e <- expr
  return (mc, e)

outValMaybe :: Expr -> Shell ()
outValMaybe e = case toValue e of
  Just v  -> out v
  Nothing -> out e

onEval :: Expr -> Shell ()
onEval e = do
  let ev = eval e
  outValMaybe ev

onList :: Expr -> Shell ()
onList e = do
  let ls = evalLs e
  mapM_  outNumExpr (zip [1..] ls)
  outValMaybe (last ls) where
    outNumExpr (n, e) = outS ("[λ][" ++ show n ++ "] " ++ show e)

onEff :: Expr -> Shell ()
onEff e = let
  mv = do
    ee <- Ad.toEff e
    Ad.toValue (Ef.eval ee) in
  case mv of
    Just v  -> out v
    Nothing -> out "Expression does not evaluate to a valid value"
