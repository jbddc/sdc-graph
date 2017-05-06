module Graph where

import Data.Maybe
import Data.List
import qualified Data.IntMap.Strict as Map
import Control.Monad.State.Lazy
import Control.Monad
import Control.Monad.Reader
import qualified Data.Set as Set

type Graph a = Map.IntMap (Set.Set a)

createGraph :: Int -> Graph Int
createGraph number = foldr (\x graph -> Map.insert x Set.empty graph) Map.empty [0..number-1]

degree :: Graph Int -> Int -> Maybe Int
degree g v = let l = Map.lookup v g in Set.size `fmap` l

degrees :: Graph Int -> [(Int,Rational)]
degrees graph = map (\k -> (k,toRational $ fromJust $ degree graph k)) $ Map.keys graph

addEdge :: Graph Int -> (Int,Int) -> Graph Int
addEdge graph (v1,v2) =
  let
    set_insert = (\x y -> Set.insert (Set.elemAt 0 x) y)
    v1_insert = Map.insertWith set_insert v1 (Set.singleton v2) graph
    v2_insert = Map.insertWith set_insert v2 (Set.singleton v1) v1_insert
    in v2_insert

verifyConnected :: Graph Int -> Bool
verifyConnected g = let
  nodes = Map.keys g
  bfs_res = concat $ runReader (breadthFirstTraverse [0]) g
  in and $ map (\v -> v `elem` bfs_res) nodes

breadthFirst :: Monad m => (Int -> m [Int]) -> [Int] -> m [[Int]]
breadthFirst f = go
  where go [] = return []
        go as = liftM (as:) . go . concat =<< mapM f as

visit :: Int -> StateT (Set.Set Int) (Reader (Graph Int)) [Int]
visit a = do
  adjacent <- asks $ Map.findWithDefault Set.empty a
  unvisited <- filterM (fmap not . gets . Set.member) $ Set.toList adjacent
  mapM_ (modify . Set.insert) unvisited 
  return unvisited

breadthFirstTraverse :: [Int] -> Reader (Graph Int) [[Int]]
breadthFirstTraverse as = breadthFirst visit as `evalStateT` Set.fromList as
