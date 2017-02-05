module Lambda where

import           Control.Monad.State
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Text.PrettyPrint    (Doc, (<+>), (<>))
import qualified Text.PrettyPrint    as P

{- Datatype definition -}
type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr

infixl 9 -$-
(-$-) :: Expr -> Expr -> Expr
f -$- x = App f x

infixr 8 -->
(-->) :: Name -> Expr -> Expr
x --> expr = Lam x expr

infixr 8 -->>
(-->>) :: [Name] -> Expr -> Expr
vs -->> expr = foldr (-->) expr vs

{- Basic operations -}
free :: Expr -> Set Name
free (Var x)      = S.singleton x
free (App e1 e2)  = S.union (free e1) (free e2)
free (Lam x expr) = S.delete x (free expr)

bound :: Expr -> Set Name
bound (Var x)      = S.empty
bound (App e1 e2)  = S.union (bound e1) (bound e2)
bound (Lam x expr) = S.insert x (bound expr)

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

genNameNotIn :: Name -> Set Name -> Name
genNameNotIn n names = let n1 = n ++ "_"
  in if S.member n1 names then genNameNotIn n1 names else n1

redex :: Expr -> Bool
redex (App (Lam x e) t) = True
redex _                 = False

reduce :: Expr -> Expr
reduce (App (Lam x e) t) = subst e x t

{- Zipper for syntax tree traversal -}
data ZExpr
  = ZNode Expr ZParent
  deriving (Show)

data ZParent
  = ZRoot
  | ZLam Name ZParent
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
  (ZLam x pp)   -> zipper (Lam x e) pp

left :: ZExpr -> ZExpr
left (ZNode (App e1 e2) p) = zipper e1 (ZAppL e2 p)
left (ZNode (Lam x e) p)   =  zipper e (ZLam x p)

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

mapZIf :: (ZExpr -> Bool) -> (ZExpr -> ZExpr) -> ZExpr -> ZExpr
mapZIf p f z
  | p z = f z
  | isVar z = mapZIfBack p f z
  | otherwise = mapZIf p f (left z) where
    mapZIfBack p f z
      | isLeft z = mapZIf p f (right (up z))
      | isRoot z = z
      | otherwise = mapZIfBack p f (up z)

normalize :: Expr -> Expr
normalize e = unzipper $ mapZIf redexZ reduceZ (root e) where
  redexZ = redex . unzipper
  reduceZ z = mapZIf redexZ reduceZ ((if isLeft z then up else id) $ applyZ reduce z)

{- Prety printing -}
varsBody :: Expr -> ([Name], Expr)
varsBody (Lam x e) = let (vars, body) = varsBody e
  in (x : vars, body)
varsBody e = ([], e)

pretty :: Expr -> Doc
pretty (Var x)   = P.text x
pretty (App e1 e2) = pretty e1 <+> parensApp (pretty e2)
  where parensApp = case e2 of (App _ _) -> P.parens
                               _         -> id
pretty lam = let (vars, body) = varsBody lam
  in P.parens $ P.char '\\' <> P.hsep (map P.text vars)
     <+> P.text "->" <+> pretty body

instance Show Expr where
  show = P.render . pretty
