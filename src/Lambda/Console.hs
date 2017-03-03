module Lambda.Console (run) where

import           Control.Monad              (unless)
import           Control.Monad.State.Strict
import           Data.List                  (intercalate)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Lambda.Console.Command
import           Lambda.Console.Context
import           Lambda.Core                (Expr, Name, toValue)
import           Lambda.Efficient.Adapter   (effEvalToValue)
import           Lambda.Eval                (eval, evalL, evalN)
import           Lambda.Parser              (def, expr, name)
import           System.Console.Haskeline
import           System.IO
import           Text.Parsec
import           Text.Parsec.String         (Parser)

type Console a = InputT (StateT Context IO) a

run :: IO ()
run = evalStateT (runInputT defaultSettings loop) empty

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

err :: String -> String
err s = "<" ++ s ++ ">"

out :: Show s => s -> Console ()
out s = outputStrLn (pref ++ show s)

outS :: String -> Console ()
outS s = outputStrLn (pref ++ s)

inp :: Console String
inp = fmap fromJust (getInputLine pref)

onCmd :: String -> Command -> Console ()
onCmd input Eval  = onEval input
onCmd input List  = onList input
onCmd input Steps = onSteps input
onCmd input Eff   = onEff input
onCmd input Set   = onSet input
onCmd input Load  = onLoad input
onCmd _     Ctx   = onCtx
onCmd input Rmv   = onRmv input
onCmd _     Clr   = onClr

onEval :: String -> Console ()
onEval input = either out outEval (parse expr "" input) where
  outEval e = do
    ec <- withCtx e
    let ev = eval ec
     in maybe (out ev) out (toValue ev)

onList :: String -> Console ()
onList input = either out outList (parse expr "" input) where
  outList e = do
    ec <- withCtx e
    let ls = evalL ec
        lls = last ls
    mapM_  (outS . showNumExpr) (zip [1..] ls)
    maybe (out lls) out (toValue (last ls))

onSteps :: String -> Console ()
onSteps input = either out outSteps (parse expr "" input) where
  outSteps e = do
    ec <- withCtx e
    let (ev, n) = evalN ec
    maybe (out ev) out (toValue ev)
    outS ("[" ++ show n ++ "]")

onEff :: String -> Console ()
onEff input = either out outEff (parse expr "" input) where
    outEff e =  do
      ec <- withCtx e
      maybe (outS $ err "invalid value") out (effEvalToValue ec)

onSet :: String -> Console ()
onSet input = either out setDef (parse def "" input) where
    setDef :: Definition -> Console ()
    setDef d = lift $ modify (`set` d)

onLoad :: String -> Console ()
onLoad input = let file = (unwords . words) input in
  if null file then outS $ err "no file specified" else do
    content <- lift . lift $ readFile file
    mapM_ onSet (lines content)

onCtx :: Console ()
onCtx = do
    defs <- lift $ gets entries
    if not (null defs)
    then mapM_ (outS . showDef) defs
    else outS $ err "context is empty"

onRmv :: String -> Console ()
onRmv input = either out rmDef (parse name "" input) where
  rmDef :: String -> Console ()
  rmDef n = lift $ modify (`remove` n)

onClr :: Console ()
onClr = lift $ put empty

withCtx :: Expr -> Console Expr
withCtx e = do
  ctx <- lift get
  return (resolve ctx e)

showNumExpr :: (Int, Expr) -> String
showNumExpr (n, e) = "[" ++ show n ++ "] " ++ show e
