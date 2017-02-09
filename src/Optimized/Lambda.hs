module Optimized.Lambda where

import           Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P

data Expr
  = Var Int
  | App Expr Expr
  | Lam Expr

infixl 9 <>
(<>) :: Expr -> Expr -> Expr
f <> x = App f x

lam :: Expr -> Expr
lam = Lam

lamn :: Int -> Expr -> Expr
lamn n e = iterate lam e !! n

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

--traverses zipper z in depth-first order (chooses left children first) applying f to z whenever p z is True
dfZCondMap :: (ZExpr -> Bool) -> (ZExpr -> ZExpr) -> ZExpr -> ZExpr
dfZCondMap p f z
  | p z       = dfZCondMap p f (f z)
  | isVar z   = dfZCondMap' p f z
  | otherwise = dfZCondMap p f (left z) where
    dfZCondMap' p f z
      | isLeft z  = dfZCondMap p f (right (up z))
      | isRoot z  = z
      | otherwise = dfZCondMap' p f (up z)

normalize :: Expr -> Expr
normalize e = unzipper $ dfZCondMap (redex . unzipper) reduceZ (root e)
  where reduceZ z = (if isLeft z then up else id) (applyZ reduce z)

{- Prety printing -}
lamBody :: Expr -> (Int, Expr)
lamBody (Lam e) = let (n, body) = lamBody e
  in (n + 1, body)
lamBody e = (0, e)

pretty :: Expr -> Doc
pretty (Var n)   = P.int n
pretty (App e1 e2) = pretty e1 P.<+> parensApp (pretty e2)
  where parensApp = case e2 of (App _ _) -> P.parens
                               _         -> id
pretty lam = let (n, body) = lamBody lam
  in P.parens $ P.char 'L' P.<> (if n > 1 then P.int n else P.empty)
     P.<> P.char '.' P.<> pretty body

instance Show Expr where
  show = P.render . pretty
