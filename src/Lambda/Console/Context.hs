module Lambda.Console.Context where

import qualified Data.Map.Strict  as M
import           Data.Maybe       (fromJust)
import qualified Data.Set         as S
import           Lambda.Core
import           Lambda.Eval
import           Lambda.Expr
import qualified Model.Graph      as G
import           Text.PrettyPrint as P (equals, render, text, (<+>), (<>))

data Context = Context
  (M.Map Name Expr)
  (G.Graph Name)
  deriving (Show)

type Definition = (Name, Expr)

empty :: Context
empty = Context M.empty G.empty

set :: Context -> Definition -> Context
set (Context ds gr) (nm, ex) = Context dsn (setTo . setFrom . reset $ gr) where
  dsn = M.insert nm ex ds
  reset gr = G.vertex (G.delete gr nm) nm
  setFrom gr = S.foldl (\g to -> G.edge g (nm, to)) gr (free ex `S.intersection` M.keysSet dsn)
  setTo gr = M.foldlWithKey (\g n e -> if S.member nm (free e) then G.edge g (n, nm) else g) gr ds

remove :: Context -> Name -> Context
remove (Context ds gr) n = Context (M.delete n ds) (G.delete gr n)

entries :: Context -> [Definition]
entries (Context ds _) = M.toList ds

resolve :: Context -> Expr -> Expr
resolve (Context ds gr) expr = foldl resolveDef expr (fromJust $ G.topSort g) where
  (g, rs) = G.toAcyclic $ G.subgraph gr $ G.search gr (free expr `S.intersection` M.keysSet ds)
  resolveDef e n = case M.lookup n rs of
    Nothing -> letIn n (fromJust $ M.lookup n ds) e
    Just s -> case S.toList s of
      [_] -> letRecIn n (fromJust $ M.lookup n ds) e
      ns -> let m = genName (free e `S.union` S.fromList ns)
             in letRecManyIn m (map (\n -> (n, fromJust $ M.lookup n ds)) ns) e

showDef :: Definition -> String
showDef (n, e) = render $ text n <+> equals <+> text (maybe (show e) show (toValue e))
