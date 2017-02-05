module Optimized.Lambda where

data Expr
  = Var Int
  | App Expr Expr
  | Lam Expr
  deriving (Show)

infixl 9 -$-
(-$-) :: Expr -> Expr -> Expr
f -$- x = App f x

lam :: Expr -> Expr
lam = Lam

lamN :: Int -> Expr -> Expr
lamN n e = iterate lam e !! n

shift :: Expr -> Int -> Expr
shift e d = shiftC e d 0

shiftC :: Expr -> Int -> Int -> Expr
shiftC (Var k) d c     = if k < c then Var k else Var (k + d)
shiftC (App t1 t2) d c = App (shiftC t1 d c) (shiftC t2 d c)
shiftC (Lam t) d c     = Lam (shiftC t d (c + 1))

subst :: Expr -> Int -> Expr -> Expr
subst (Var k) j s     = if j == k then s else Var k
subst (App t1 t2) j s = App (subst t1 j s) (subst t2 j s)
subst (Lam t) j s     = Lam (subst t (j + 1) (shift s 1))

redex :: Expr -> Bool
redex (App (Lam t) v) = True
redex _               = False

reduce :: Expr -> Expr
reduce (App (Lam t) v) = shift (subst t 0 (shift v 1)) (-1)

{- Zipper for syntax tree traversal -}
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

mapZIf :: (ZExpr -> Bool) -> (ZExpr -> ZExpr) -> ZExpr -> ZExpr
mapZIf p f z
  | p z = f z
  | isVar z = mapZIfBack p f z
  | otherwise = mapZIf p f (left z) where
    mapZIfBack p f z
      | isLeft z = mapZIf p f (right (up z))
      | isRoot z = z
      | otherwise = mapZIfBack p f z

normalize :: Expr -> Expr
normalize e = unzipper $ mapZIf redexZ reduceZ (root e) where
  redexZ = redex . unzipper
  reduceZ z = mapZIf redexZ ((if isLeft z then up else id) . applyZ reduce) z
