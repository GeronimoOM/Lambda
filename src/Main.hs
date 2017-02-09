module Main where

import           Lambda.Core
import           Lambda.Eval
import           Lambda.Expr   as Ex
import           Lambda.Parser as Ps
import           Lambda.Print

main :: IO ()
main = do
  parseTest
  return ()

parseTest :: IO ()
parseTest = do
  str <- getLine
  let me = parseExpr str
  case me of
    Left err -> print err
    Right e  -> do
      let ev = eval e
      print (pexpr e)
      print (pexpr ev)
      case value ev of
        Just v  -> print v
        Nothing -> return ()
