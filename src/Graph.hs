module Graph where

import Data.Maybe
import Data.List
import qualified Data.IntMap.Strict as Map
import Control.Monad.State.Lazy
import Control.Monad
import Control.Monad.Reader
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import Data.GraphViz.Types
import Data.GraphViz.Commands
import Data.GraphViz.Types.Generalised

type Graph a = Map.IntMap (Set.Set a)

createGraph :: Int -> Graph Int
createGraph number = foldr (\x graph -> Map.insert x Set.empty graph) Map.empty [0..number-1]

degree :: Graph Int -> Int -> Maybe Int
degree g v = let l = Map.lookup v g in Set.size `fmap` l

degrees :: Graph Int -> [(Int,Int)]
degrees graph = map (\k -> (k,(1+) $ fromJust $ degree graph k)) $ Map.keys graph

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

graphVizRepresentation :: (Graph Int) -> (Map.IntMap Bool) -> String
graphVizRepresentation g colors =
  let nodes = Map.keys g
      header = "graph G {\n"
      footer = "}\n"
      colorList = Map.foldrWithKey (\key b accum -> if b then (colorize key "blue")++accum else accum) [] colors
      middle = Map.foldrWithKey foldr_aux [] g 
  in header++colorList++middle++footer
  where
    foldr_aux node neighbours accum = accum++ (concat $ Set.map (\x -> edgerize node x) neighbours)
    colorize n color = "\""++(show n)++"\" "++"["++"style=filled,color="++color++"];\n" 
    edgerize l r = if l<r then "  "++(show l)++" -- "++(show r)++";\n" else ""

publishGraphVizRepr :: String -> FilePath -> IO FilePath
publishGraphVizRepr s fp = 
  let g = (parseDotGraph $ Text.pack s) :: DotGraph String 
  in runGraphviz g Png fp