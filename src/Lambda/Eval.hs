module Lambda.Eval
  (free, bound, subst, genName, redex, reduce, eval, evalL)
where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Lambda.Core

free :: Expr -> Set Name
free (Var x)     = S.singleton x
free (App e1 e2) = S.union (free e1) (free e2)
free (Lam x e)   = S.delete x (free e)

bound :: Expr -> Set Name
bound (Var x)     = S.empty
bound (App e1 e2) = S.union (bound e1) (bound e2)
bound (Lam x e)   = S.insert x (bound e)

subst :: Expr -> Name -> Expr -> Expr
subst (Var x) v t
  | x == v = t
  | otherwise = Var x
subst (App e1 e2) v t = App (subst e1 v t) (subst e2 v t)
subst (Lam x e) v t
  | x == v = Lam x e
  | S.notMember v (free e) || S.notMember x (free t) = Lam x (subst e v t)
  | otherwise = let z = genName (S.union (free e) (free t))
                in Lam z (subst (subst e x (Var z)) v t)

genName :: Set Name -> Name
genName names = genName' names 'a' where
  genName' ns n = if S.member [n] ns then genName' ns (succ n) else [n]

redex :: Expr -> Bool
redex (App (Lam x e) t) = True
redex _                 = False

reduce :: Expr -> Expr
reduce (App (Lam x e) t) = subst e x t

eval :: Expr -> Expr
eval e = case evalSRun e
   of (ev, False) -> ev
      (ev, True)  -> eval ev

evalSRun :: Expr -> (Expr, Bool)
evalSRun e = runState (evalSStop e) False

evalSStop :: Expr -> State Bool Expr
evalSStop e = do
  stop <- get
  if stop then return e else evalSTrav e

evalSTrav :: Expr -> State Bool Expr
evalSTrav e | redex e = evalSRed e
evalSTrav (App e1 e2) = do
  e1e <- evalSStop e1
  e2e <- evalSStop e2
  return (App e1e e2e)
evalSTrav (Lam n e) = do
  ee <- evalSStop e
  return (Lam n ee)
evalSTrav e = return e

evalSRed :: Expr -> State Bool Expr
evalSRed e = do
  put True
  return (reduce e)

evalL :: Expr -> [Expr]
evalL e = execWriter (evalW e)

evalW :: Expr -> Writer [Expr] Expr
evalW e = do
  tell [e]
  case evalSRun e of
    (ev, False) -> return ev
    (ev, True)  ->  evalW ev
