module Lambda.Eval
  (free, bound, subst, redex, reduce, eval)
where

import qualified Data.Set    as S
import           Lambda.Core

free :: Expr -> S.Set Name
free (Var x)     = S.singleton x
free (App e1 e2) = S.union (free e1) (free e2)
free (Lam x e)   = S.delete x (free e)

bound :: Expr -> S.Set Name
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
  | otherwise = let z = genNameNotIn x (S.union (free e) (free t))
                in Lam z (subst (subst e x (Var z)) v t)

genNameNotIn :: Name -> S.Set Name -> Name
genNameNotIn n names = let n' = n ++ "_"
  in if S.member n' names then genNameNotIn n' names else n'

redex :: Expr -> Bool
redex (App (Lam x e) t) = True
redex _                 = False

reduce :: Expr -> Expr
reduce (App (Lam x e) t) = subst e x t

{- Zipper for syntax tree traversal -}
data ZExpr
  = ZNode Expr ZParent

data ZParent
  = ZRoot
  | ZLam Name ZParent
  | ZAppL Expr ZParent
  | ZAppR Expr ZParent

zipper :: Expr -> ZParent -> ZExpr
zipper = ZNode

root :: Expr -> ZExpr
root e = zipper e ZRoot

up :: ZExpr -> ZExpr
up (ZNode e p) = case p of
  (ZAppL e2 pp) -> zipper (App e e2) pp
  (ZAppR e1 pp) -> zipper (App e1 e) pp
  (ZLam x pp)   -> zipper (Lam x e) pp

left :: ZExpr -> ZExpr
left (ZNode (App e1 e2) p) = zipper e1 (ZAppL e2 p)
left (ZNode (Lam x e) p)   = zipper e (ZLam x p)

right :: ZExpr -> ZExpr
right (ZNode (App e1 e2) p) = zipper e2 (ZAppR e1 p)

isRoot :: ZExpr -> Bool
isRoot (ZNode _ ZRoot) = True
isRoot _               = False

isLeft :: ZExpr -> Bool
isLeft (ZNode _ ZAppL{}) = True
isLeft _                 = False

isVar :: ZExpr -> Bool
isVar (ZNode Var{} _) = True
isVar _               = False

unzipper :: ZExpr -> Expr
unzipper (ZNode e _) = e

applyZ :: (Expr -> Expr) -> ZExpr -> ZExpr
applyZ f (ZNode e p) = ZNode (f e) p

--traverses zipper z in depth-first order (chooses left child first) applying f to z whenever (p z) is True
dfZCondMap :: (ZExpr -> Bool) -> (ZExpr -> ZExpr) -> ZExpr -> ZExpr
dfZCondMap p f z
  | p z = dfZCondMap p f (f z)
  | isVar z =   dfZCondMap' p f z
  | otherwise = dfZCondMap p f (left z) where
    dfZCondMap' p f z
      | isLeft z = dfZCondMap p f (right (up z))
      | isRoot z = z
      | otherwise = dfZCondMap' p f (up z)

eval :: Expr -> Expr
eval e = unzipper $ dfZCondMap (redex . unzipper) reduceZ (root e)
  where reduceZ z = (if isLeft z then up else id) (applyZ reduce z)
