module Optimized.Lambda where

data Expr
  = Var Int
  | App Expr Expr
  | Lam Expr

shift :: Expr -> Int -> Expr
shift e d = shiftC e d 0

shiftC :: Expr -> Int -> Int -> Expr
shiftC (Var k) d c     = if k < c then Var k else Var (k + d)
shiftC (App t1 t2) d c = App (shiftC t1 d c) (shiftC t2 d c)
shiftC (Lam t) d c     = Lam (shiftC t d (c + 1))

subst :: Expr -> Int -> Expr -> Expr
subst (Var k) j s = if j == k then s else Var k
subst (App t1 t2) j s = App (subst t1 j s) (subst t2 j s)
subst (Lam t) j s = Lam (subst t (j + 1) (shift s 1))
