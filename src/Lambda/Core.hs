module Lambda.Core
  (Expr(..), Name, (<>), (-->), (-->>), Value(..), toValue) where

import           Text.PrettyPrint (char, hsep, parens, render, text, (<+>))
import qualified Text.PrettyPrint as P


data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr

type Name = String

infixl 9 <>
(<>) :: Expr -> Expr -> Expr
f <> x = App f x

infixr 8 -->
(-->) :: Name -> Expr -> Expr
x --> e = Lam x e

infixr 8 -->>
(-->>) :: [Name] -> Expr -> Expr
vs -->> e = foldr (-->) e vs


instance Show Expr where
  show = P.render . pretty

pretty :: Expr -> P.Doc
pretty (Var x)   = text x
pretty (App e1 e2) = parensLam e1 (pretty e1)
  <+> parensNotVar e2 (pretty e2) where
    parensLam (Lam _ _) = parens
    parensLam _         = id
    parensNotVar  (Var _) = id
    parensNotVar _        = parens
pretty lam = let (vars, body) = varsBody lam
  in char 'λ' P.<> hsep (map text vars)
     <+> text "->" <+> pretty body

varsBody :: Expr -> ([Name], Expr)
varsBody (Lam x e) = let (vars, body) = varsBody e
 in (x : vars, body)
varsBody e = ([], e)



data Value
  = VBool Bool
  | VNat Int
  | VPair (Value, Value)

instance Show Value where
  show (VBool b) = show b
  show (VNat i)  = show i
  show (VPair p) = show p

toValue :: Expr -> Maybe Value
toValue (Lam x (Lam y (Var vx))) | x == vx = return (VBool True)
toValue (Lam f (Lam x e)) = do
  n <- count e
  return (VNat n) where
    count (Var x') | x' == x = return 0
    count (App (Var f') e') | f' == f = do
      n <- count e'
      return (n + 1)
    count _            = Nothing
toValue (Lam f (App (App (Var vf) lt) rt)) | f == vf = do
  vlt <- toValue lt
  vrt <- toValue rt
  return (VPair (vlt, vrt))
toValue _ = Nothing
