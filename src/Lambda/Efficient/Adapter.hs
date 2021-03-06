module Lambda.Efficient.Adapter where

import           Data.List
import           Lambda.Core      (Expr (..), Value (..))
import qualified Lambda.Efficient as Ef

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
toValue (Ef.Lam (Ef.Lam e)) = do
  n <- count e
  return (VNat n) where
    count (Ef.Var 0)  = return 0
    count (Ef.App (Ef.Var 1) e) = do
      n <- count e
      return (n + 1)
    count _            = Nothing
toValue (Ef.Lam (Ef.App (Ef.App (Ef.Var 0) lt) rt)) = do
  vlt <- toValue lt
  vrt <- toValue rt
  return (VPair (vlt, vrt))
toValue _ = Nothing

effEvalToValue :: Expr -> Maybe Value
effEvalToValue e = do
  ef <- toEff e
  toValue (Ef.eval ef)

effEvalNToValue :: Expr -> Maybe (Value, Int)
effEvalNToValue e = do
  ef <- toEff e
  let (ev, n) = Ef.evalN ef
  val <- toValue ev
  return (val, n)
