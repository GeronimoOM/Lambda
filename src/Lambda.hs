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


{- Zipper for syntax tree traversal -}
data ZExpr
  = ZRoot
  | ZVar ZExpr Name
  | ZApp ZExpr Expr Expr
  | ZLam ZExpr Name Expr
  deriving (Show)

root :: Expr -> ZExpr
root e = zipper e ZRoot

zipper :: Expr -> ZExpr -> ZExpr
zipper (Var x)     p = ZVar p x
zipper (App e1 e2) p = ZApp p e1 e2
zipper (Lam x e)   p = ZLam p x e

parent :: ZExpr -> ZExpr
parent (ZVar p _)   = p
parent (ZApp p _ _) = p
parent (ZLam p _ _) = p

scope :: ZExpr -> Expr
scope (ZVar _ x)     = Var x
scope (ZApp _ e1 e2) = App e1 e2
scope (ZLam _ x e)   = Lam x e

up :: ZExpr -> ZExpr
up e = case parent e of
  (ZApp p (Var "") e2) -> ZApp p (scope e) e2
  (ZApp p e1 (Var "")) -> ZApp p e1 (scope e)
  (ZLam p x e1)        -> ZLam p x (scope e)

down :: ZExpr -> ZExpr
down (ZLam p x e) = zipper e (ZLam p x (Var ""))

left :: ZExpr -> ZExpr
left (ZApp p e1 e2) = zipper e1 (ZApp p (Var "") e2)

right :: ZExpr -> ZExpr
right (ZApp p e1 e2) = zipper e2 (ZApp p e1 (Var ""))

isRoot :: ZExpr -> Bool
isRoot z = case parent z
  of ZRoot -> True
     _     -> False

isLeft :: ZExpr -> Bool
isLeft z = case parent z
  of (ZApp p (Var "") e2) -> True
     _                    -> False

isRight :: ZExpr -> Bool
isRight z = case parent z
 of (ZApp p e1 (Var "")) -> True
    _                    -> False

unzipper :: ZExpr -> Expr
unzipper e = case
  parent e of ZRoot -> scope e
              _     -> unzipper (up e)

applyZ :: ZExpr -> (Expr -> Expr) -> ZExpr
applyZ z f = zipper (f (scope z)) (parent z)

redex :: Expr -> Bool
redex (App (Lam x e) t) = True
redex _                 = False

reduce :: Expr -> Expr
reduce (App (Lam x e) t) = subst e x t

normalize :: Expr -> Expr
normalize e = unzipper (normalizeZ (root e))

normalizeZ :: ZExpr -> ZExpr
normalizeZ z | redex (scope z) =
    let z' = applyZ z reduce
    in if isLeft z' then normalizeZ $ up z'
       else normalizeZ z'
normalizeZ z@ZLam{} = normalizeZ (down z)
normalizeZ z@ZApp{} = normalizeZ (left z)
normalizeZ z@ZVar{} = normalizeZBack z

normalizeZBack :: ZExpr -> ZExpr
normalizeZBack z
  | isLeft z  = normalizeZ (right (up z))
  | isRoot z  = z
  | otherwise = normalizeZBack (up z)

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
