module Lambda.Efficient.Nameless (Expr(..), shift, shiftc, subst, redex, reduce, eval) where

data Expr
  = Var Int
  | App Expr Expr
  | Lam Expr
  deriving (Show)

shift :: Expr -> Int -> Expr
shift e d = shiftc e d 0

shiftc :: Expr -> Int -> Int -> Expr
shiftc (Var k) d c     = if k < c then Var k else Var (k + d)
shiftc (App t1 t2) d c = App (shiftc t1 d c) (shiftc t2 d c)
shiftc (Lam t) d c     = Lam (shiftc t d (c + 1))

subst :: Expr -> Int -> Expr -> Expr
subst (Var k) j s     = if j == k then s else Var k
subst (App t1 t2) j s = App (subst t1 j s) (subst t2 j s)
subst (Lam t) j s     = Lam (subst t (j + 1) (shift s 1))

redex :: Expr -> Bool
redex (App (Lam t) v) = True
redex _               = False

reduce :: Expr -> Expr
reduce (App (Lam t) v) = shift (subst t 0 (shift v 1)) (-1)
reduce e               = e

data ZExpr
  = ZNode Expr ZParent
  deriving (Show)

data ZParent
  = ZRoot
  | ZLam ZParent
  | ZAppL Expr ZParent
  | ZAppR Expr ZParent
    deriving (Show)

zipper :: Expr -> ZParent -> ZExpr
zipper = ZNode

root :: Expr -> ZExpr
root e = zipper e ZRoot

up :: ZExpr -> ZExpr
up (ZNode e p) = case p of
  (ZAppL e2 pp) -> zipper (App e e2) pp
  (ZAppR e1 pp) -> zipper (App e1 e) pp
  (ZLam pp)     -> zipper (Lam e) pp

left :: ZExpr -> ZExpr
left (ZNode (App e1 e2) p) = zipper e1 (ZAppL e2 p)
left (ZNode (Lam e) p)     =  zipper e (ZLam p)

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
applyZ f (ZNode e p) = zipper (f e) p

dfsZ :: (ZExpr -> Bool) -> ZExpr -> Either ZExpr ZExpr
dfsZ p z
  | p z = Right z
  | isVar z =   dfsZ' p z
  | otherwise = dfsZ p (left z) where
    dfsZ' p z
      | isLeft z = dfsZ p (right (up z))
      | isRoot z = Left z
      | otherwise = dfsZ' p (up z)

eval :: Expr -> Expr
eval e = unzipper $ dfsRedex (root e) where
  dfsRedex z = case dfsZ (redex . unzipper) z
    of Right z -> dfsRedex $ (if isLeft z then up else id) (applyZ reduce z)
       Left z  -> z
