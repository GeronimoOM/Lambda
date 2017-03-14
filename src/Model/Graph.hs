module Model.Graph where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Prelude         hiding (null)

type Graph v = Map v (Set v)

empty :: Graph v
empty = M.empty

null :: Graph v -> Bool
null = M.null

verts :: Graph v -> Set v
verts = M.keysSet

out :: (Ord v) => Graph v -> v -> Set v
out g v = fromMaybe S.empty (M.lookup v g)

inb :: (Ord v) => Graph v -> v -> Set v
inb g v = M.foldlWithKey (\t w fr -> if S.member v fr then S.insert w t else t) S.empty g

insert :: (Ord v) => Graph v -> v -> Graph v
insert g v = M.insert v S.empty g

edge :: (Ord v) => Graph v -> (v, v) -> Graph v
edge g (from, to) = M.insertWith S.union from (S.singleton to) g

delete :: (Ord v) => Graph v -> v -> Graph v
delete g v = M.delete v $ M.map (`S.difference` S.singleton v) g

search :: (Ord v) => Graph v -> Set v -> Set v
search g vs = searchVis g vs S.empty where
  searchVis g op cl = if S.null op then cl
    else let (v, vs) = S.deleteFindMin op in
     searchVis g (S.union vs (out g v S.\\ cl)) (S.insert v cl)

subgraph :: (Ord v) => Graph v -> Set v -> Graph v
subgraph g vs = S.foldl delete g (verts g S.\\ vs)

delDrains :: (Ord v) => Graph v -> Graph v
delDrains g = maybe g (delDrains . delete g) (findDrain g)

findDrain :: Graph v -> Maybe v
findDrain g = findDrainAsc (M.toAscList g) where
  findDrainAsc ((v, to) : es) = if S.null to then Just v else findDrainAsc es
  findDrainAsc []             = Nothing

topSort :: (Ord v) => Graph v -> Maybe [v]
topSort g = if null g then return []
  else do
    v <- findDrain g
    g' <- topSort (delete g v)
    return (g' ++ [v])
