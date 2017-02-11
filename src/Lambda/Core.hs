module Lambda.Core where

import           Text.PrettyPrint as P

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
  show = P.render .doc

doc :: Expr -> Doc
doc (Var x)   = text x
doc (App e1 e2) = parensLam e1 (doc e1)
  <+> parensNotVar e2 (doc e2) where
    parensLam (Lam _ _) = parens
    parensLam _         = id
    parensNotVar  (Var _) = id
    parensNotVar _        = parens
doc lam = let (vars, body) = varsBody lam
  in char 'λ' P.<> hsep (map text vars)
     <+> text "->" <+> doc body

varsBody :: Expr -> ([Name], Expr)
varsBody (Lam x e) = let (vars, body) = varsBody e
 in (x : vars, body)
varsBody e = ([], e)

data Value
  = VBool Bool
  | VNat Int

instance Show Value where
  show (VBool b) = show b
  show (VNat i)  = show i

toValue :: Expr -> Maybe Value
toValue (Lam x (Lam y (Var vx))) | x == vx = return (VBool True)
toValue (Lam x (Lam y (Var vy))) | y == vy = return (VBool False)
toValue (Lam f (Lam x e)) = do
  n <- count e
  return (VNat n) where
    count (Var x') | x' == x = return 0
    count (App (Var f') e') | f' == f = do
      n <- count e'
      return (n + 1)
    count _            = Nothing
toValue _ = Nothing
