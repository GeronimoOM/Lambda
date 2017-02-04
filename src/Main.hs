module Main where

import           Lambda
import           Lambda.Expressions
import           Prelude            hiding (and, snd)

main :: IO ()
main = do
  let e = fact -$- num 4
  print e

  let ne = normalize e
  print ne

  getChar
  return ()
