module Lambda.Core where

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  deriving (Eq, Show)

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

data Value
  = VBool Bool
  | VInt Int

instance Show Value where
  show (VBool b) = show b
  show (VInt i)  = show i

value :: Expr -> Maybe Value
value (Lam x (Lam y (Var vx))) | x == vx = return (VBool True)
value (Lam x (Lam y (Var vy))) | y == vy = return (VBool False)
value (Lam f (Lam x e)) = do
  n <- count e
  return (VInt n) where
    count (Var x') | x' == x = return 0
    count (App (Var f') e') | f' == f = do
      n <- count e'
      return (n + 1)
    count _            = Nothing
value _ = Nothing
