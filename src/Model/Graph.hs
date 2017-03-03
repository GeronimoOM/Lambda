module Model.Graph where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Prelude         hiding (null)

type Graph a = Map a (Set a)

empty :: Graph a
empty = M.empty

null :: Graph a -> Bool
null = M.null

verts :: Graph a -> Set a
verts = M.keysSet

to :: (Ord a) => Graph a -> a -> Set a
to g v = fromMaybe S.empty (M.lookup v g)

from :: (Ord a) => Graph a -> a -> Set a
from g v = M.foldlWithKey (\t w fr -> if S.member v fr then S.insert w t else t) S.empty g

vertex :: (Ord a) => Graph a -> a -> Graph a
vertex g v = M.insert v S.empty g

edge :: (Ord a) => Graph a -> (a, a) -> Graph a
edge g (from, to) = M.insertWith S.union from (S.singleton to) g

delete :: (Ord a) => Graph a -> a -> Graph a
delete g v = M.delete v $ M.map (`S.difference` S.singleton v) g

search :: (Ord a) => Graph a -> Set a -> Set a
search g vs = searchVis g vs S.empty where
  searchVis g op cl = if S.null op then cl
    else let (v, vs) = S.deleteFindMin op in
     searchVis g (S.union vs (to g v S.\\ cl)) (S.insert v cl)

subgraph :: (Ord a) => Graph a -> Set a -> Graph a
subgraph g vs = S.foldl delete g (verts g S.\\ vs)

cycles :: (Ord a) => Graph a -> [Set a]
cycles g = if M.null (delDrains g) then []
  else cyclesComp g (S.map (search g . S.singleton) (verts g)) where
    cyclesComp g srch = let (s, ss) = S.deleteFindMin srch
      in if any (`S.isProperSubsetOf` s) ss then cyclesComp g ss
         else s : cycles (S.foldl delete g s)

delDrains :: (Ord a) => Graph a -> Graph a
delDrains g = maybe g (delDrains . delete g) (findDrain g)

findDrain :: Graph a -> Maybe a
findDrain g = findDrainAsc (M.toAscList g) where
  findDrainAsc ((v, to) : es) = if S.null to then Just v else findDrainAsc es
  findDrainAsc []             = Nothing

topSort :: (Ord a) => Graph a -> Maybe [a]
topSort g = if null g then return []
  else do
    v <- findDrain g
    g' <- topSort (delete g v)
    return (g' ++ [v])

toAcyclic :: (Ord a) => Graph a -> (Graph a, Map a (Set a))
toAcyclic g = foldl replace (g, M.empty) (cycles g) where
  replace :: (Ord a) => (Graph a, Map a (Set a)) -> Set a -> (Graph a, Map a (Set a))
  replace (g, rs) cyc = (replaceFr . replaceTo $ vertex (S.foldl delete g cyc) c, M.insert c cyc rs) where
    (c, _) = S.deleteFindMin cyc
    replaceFr h = S.foldl (\g t -> edge g (c, t)) h (S.foldl S.union S.empty (S.map (from g) cyc) S.\\ cyc)
    replaceTo h = S.foldl (\g f -> edge g (f, c)) h (S.foldl S.union S.empty (S.map (to g) cyc) S.\\ cyc)
