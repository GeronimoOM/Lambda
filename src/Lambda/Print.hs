module Lambda.Print where

import           Lambda.Core      (Expr (..), Name)
import           Text.PrettyPrint

varsBody :: Expr -> ([Name], Expr)
varsBody (Lam x e) = let (vars, body) = varsBody e
  in (x : vars, body)
varsBody e = ([], e)

doc :: Expr -> Doc
doc (Var x)   = text x
doc (App e1 e2) = parensLam e1 (doc e1)
  <+> parensNotVar e2 (doc e2) where
    parensLam (Lam _ _) = parens
    parensLam _         = id
    parensNotVar  (Var _) = id
    parensNotVar _        = parens
doc lam = let (vars, body) = varsBody lam
  in char '\\' <> hsep (map text vars)
     <+> text "->" <+> doc body

pexpr :: Expr -> String
pexpr  = render . doc
