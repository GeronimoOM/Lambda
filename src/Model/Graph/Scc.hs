module Model.Graph.Scc (strConnComp, toAcyclic) where

import           Control.Monad.State.Strict
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust, isJust)
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Model.Graph

-- Tarjan's Strongly Connected Components Algorithm
strConnComp :: (Ord v) => Graph v -> [Set v]
strConnComp g = evalState (sccAll g) initSt

sccAll :: (Ord v) => Graph v -> State (SccState v) [Set v]
sccAll g = foldM (sccConn g) [] (verts g)

sccConn :: (Ord v) => Graph v -> [Set v] -> v ->  State (SccState v) [Set v]
sccConn g sccs v = do
  isVis <- gets (isVisited v)
  if isVis then return sccs
  else do
    modify (add v)
    sccs' <- foldM (sccDfs g v) sccs (out g v)
    isRt <- gets (isRoot v)
    if isRt then do
      scc <- sccColl v
      return (scc : sccs')
    else return sccs'

sccDfs :: (Ord v) => Graph v -> v -> [Set v] -> v ->  State (SccState v) [Set v]
sccDfs g v sccs w = do
  isVis <- gets (isVisited w)
  if not isVis then do
    sccs' <- sccConn g sccs w
    modify (updMin v w)
    return sccs'
   else do
    elemStack <- gets (elem w . stack)
    unless (not elemStack) $ modify (updMin v w)
    return sccs

sccColl :: (Ord v) => v -> State (SccState v) (Set v)
sccColl v = do
  (w, st) <- gets popStack
  put st
  if v == w then return (S.singleton w)
    else do
    scc <- sccColl v
    return (S.insert w scc)

data SccState v = SccState {
  inds  :: Map v Int,
  mins  :: Map v Int,
  stack :: [v],
  index :: Int
}

initSt :: SccState v
initSt = SccState M.empty M.empty [] 0

add :: (Ord v) => v -> SccState v -> SccState v
add v (SccState inds mins st i) = SccState (M.insert v i inds) (M.insert v i mins) (v : st) (i + 1)

updMin :: (Ord v) => v -> v -> SccState v -> SccState v
updMin v w (SccState inds mins st i) = SccState inds (M.insertWith min v (fromJust $ M.lookup w mins) mins) st i

isVisited :: (Ord v) => v -> SccState v -> Bool
isVisited v st = isJust (M.lookup v (mins st))

isRoot :: (Ord v) => v -> SccState v -> Bool
isRoot v st = fromJust (M.lookup v (inds st)) == fromJust (M.lookup v (mins st))

popStack :: SccState v -> (v, SccState v)
popStack (SccState inds mins (v : s) i) = (v, SccState inds mins s i)


toAcyclic :: (Ord v) => Graph v -> (Graph v, Map v (Set v))
toAcyclic g = foldl replace (g, M.empty) (strConnComp g) where
  replace :: (Ord v) => (Graph v, Map v (Set v)) -> Set v -> (Graph v, Map v (Set v))
  replace (g, rs) cyc = (replaceInb . replaceOut $ insert (S.foldl delete g cyc) c, M.insert c cyc rs) where
    (c, _) = S.deleteFindMin cyc
    replaceInb h = S.foldl (\g t -> edge g (c, t)) h (S.foldl S.union S.empty (S.map (inb g) cyc) S.\\ cyc)
    replaceOut h = S.foldl (\g f -> edge g (f, c)) h (S.foldl S.union S.empty (S.map (out g) cyc) S.\\ cyc)
