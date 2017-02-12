module Lambda.Console (run) where

import           Control.Monad              (unless)
import           Control.Monad.State.Strict
import           Data.List                  (intercalate)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Lambda.Console.Command
import           Lambda.Core                (Expr, Name, toValue)
import           Lambda.Efficient.Adapter   (effEvalToValue)
import           Lambda.Eval                (eval, evalLs)
import           Lambda.Parser              (definit, expr)
import           System.Console.Haskeline
import           Text.Parsec                (char, parse, spaces)
import           Text.Parsec.String         (Parser)

type Console a = InputT (StateT Context IO) a
type Context = [Definition]
type Definition = (Name, Expr)

emptyCtxt :: Context
emptyCtxt = []

name :: Definition -> Name
name (n, _) = n


run :: IO ()
run = evalStateT (runInputT defaultSettings loop) emptyCtxt

loop :: Console ()
loop = do
  input <- inp
  case parse cmdInput "" input of
    Left err -> out err >> loop
    Right (mc, input) ->
      let c = fromMaybe Eval mc
       in unless (c == Quit) $
          do onCmd input c
             loop

pref :: String
pref = "[λ] "

out :: Show s => s -> Console ()
out s = outputStrLn (pref ++ show s)

outS :: String -> Console ()
outS s = outputStrLn (pref ++ s)

inp :: Console String
inp = fmap fromJust (getInputLine pref)

onCmd :: String -> Command -> Console ()
onCmd input Eval = onEval input
onCmd input List = onList input
onCmd input Eff  = onEff input
onCmd input Add  = onAdd input
onCmd _ Ctx      = onCtx
onCmd _ Mogo     = outS secret

onEval :: String -> Console ()
onEval input = either out outEval (parse expr "" input) where
  outEval e = let ev = eval e
    in maybe (out ev) out (toValue ev)

onList :: String -> Console ()
onList input = either out outList (parse expr "" input) where
  outList e = do
    let ls = evalLs e
        lls = last ls
    mapM_  outNumExpr (zip [1..] ls)
    maybe (out lls) out (toValue (last ls)) where
      outNumExpr (n, e) = outS ("[" ++ show n ++ "] " ++ show e)

onEff :: String -> Console ()
onEff input = either out outEff (parse expr "" input) where
    outEff e = maybe (outS invalid) out (effEvalToValue e)
    invalid = "expression does not evaluate to a valid value"

onAdd :: String -> Console ()
onAdd input = either out addDef (parse definit "" input) where
    addDef :: Definition -> Console ()
    addDef d      = lift $ modify (d:)

onCtx :: Console ()
onCtx = do
    ctx <- lift get
    outS (intercalate ", " (map name ctx))

secret = "'The only language better than C++ is Haskell' - Boublik V. V, 2017"
