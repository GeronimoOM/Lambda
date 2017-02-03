module Lambda where

import           Control.Monad.State
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Text.PrettyPrint    (Doc, (<+>), (<>))
import qualified Text.PrettyPrint    as P

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

free :: Expr -> Set Name
free (Var x)      = S.singleton x
free (App e1 e2)  = S.union (free e1) (free e2)
free (Lam x expr) = S.delete x (free expr)

bound :: Expr -> Set Name
bound (Var x)      = S.empty
bound (App e1 e2)  = S.union (bound e1) (bound e2)
bound (Lam x expr) = S.insert x (bound expr)

data Subst = Subst Name Expr

subst :: Expr -> Subst -> Expr
subst (Var x) (Subst v t)
  | x == v = t
  | otherwise = Var x
subst (App e1 e2) s = App (subst e1 s) (subst e2 s)
subst (Lam x e) s@(Subst v t)
  | x == v = Lam x e
  | S.notMember v (free e) || S.notMember x (free t) = Lam x (subst e s)
  | otherwise = let z = genNameNotIn x (S.union (free e) (free t))
                in Lam z (subst (subst e (Subst x (Var z))) s)

genNameNotIn :: Name -> Set Name -> Name
genNameNotIn n names = let n1 = n ++ "_"
  in if S.member n1 names then genNameNotIn n1 names else n1

normalize :: Expr -> Expr
normalize e = case runState (reduceS e) False
  of (e', False) -> e'
     (e', True)  -> normalize e'

reduceS :: Expr -> State Bool Expr
reduceS e = do
  s <- get
  if s then return e
       else reduceS' e

reduceS' :: Expr -> State Bool Expr
reduceS' (App (Lam x e) t) = do
  put True
  return $ subst e (Subst x t)
reduceS' (App e1 e2)  = do
  e1' <- reduceS e1
  e2' <- reduceS e2
  return $ App e1' e2'
reduceS' (Lam x expr) = do
  expr' <- reduceS expr
  return $ Lam x expr'
reduceS' e = return e

varsBody :: Expr -> ([Name], Expr)
varsBody (Lam x e) = let (vars, body) = varsBody e
  in (x : vars, body)
varsBody e = ([], e)

parensIf :: Bool -> Doc -> Doc
parensIf True  = P.parens
parensIf False = id

prettyDp :: Expr -> Int -> Doc
prettyDp (Var x) _     = P.text x
prettyDp (App e1 e2) d = parensIf (d > 0) (prettyDp e1 (d + 1) <+> prettyDp e2 d)
prettyDp f@(Lam x e2) d = let (vars, body) = varsBody f
  in P.char '\\' <> P.hsep (map P.text vars)
    <+> P.text "->" <+> prettyDp body (d + 1)

pretty :: Expr -> Doc
pretty e = prettyDp e 0

instance Show Expr where
  show = P.render . pretty

{-
instance Show Expr where
  show (Name x)                = [x]
  show (App (Name x) (Name y)) = [x, ' ', y]
  show (App (Name x) e2)      = [x, ' '] ++ par (show e2)
  show (App e1 (Name y))      = par (show e1) ++ [' ', y]
  show (App e1 e2)           = par(show e1) ++ " " ++ par (show e2)
  show e          = maybe (maybe showLam show (maybeNum e)) show (maybeBool e)
    where showLam = "L" ++ intersperse ' ' vars ++ "." ++ show rest
          (vars, rest) = multi e
          multi (Lam x expr) = let (vs, e) = multi expr in (x : vs, e)
          multi e             = ([], e)

maybeBool :: Expr -> Maybe Bool
maybeBool (Lam x (Lam y (Name z)))
  | x == y = Nothing
  | x == z = Just True
  | y == z = Just False
maybeBool _ = Nothing

maybeNum :: Expr -> Maybe Int
maybeNum (Lam f (Lam x e)) = maybeNumCount f x e
  where maybeNumCount f x (Name x') | x == x' = Just 0
        maybeNumCount f x (App (Name f') e) | f == f' = do
          n <- maybeNumCount f x e
          return (n + 1)
        maybeNumCount f x _ = Nothing
maybeNum _                   = Nothing
-}
