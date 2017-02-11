module Lambda.Efficient.Adapter where

import           Data.List
import           Lambda.Core
import qualified Lambda.Efficient.Nameless as Ef

toEff :: Expr -> Maybe Ef.Expr
toEff = toEffSt [] where
  toEffSt st (Var x) = do
    i <- elemIndex x st
    return (Ef.Var i)
  toEffSt st (App e1 e2) = do
    e1e <- toEffSt st e1
    e2e <- toEffSt st e2
    return (Ef.App e1e e2e)
  toEffSt st (Lam n e) = do
    ee <- toEffSt (n:st) e
    return (Ef.Lam ee)

toValue :: Ef.Expr -> Maybe Value
toValue (Ef.Lam (Ef.Lam (Ef.Var 1))) = return (VBool True)
toValue (Ef.Lam (Ef.Lam (Ef.Var 0))) = return (VBool False)
toValue (Ef.Lam (Ef.Lam e)) = do
  n <- count e
  return (VNat n) where
    count (Ef.Var 0)  = return 0
    count (Ef.App (Ef.Var 1) e) = do
      n <- count e
      return (n + 1)
    count _            = Nothing
value _ = Nothing
