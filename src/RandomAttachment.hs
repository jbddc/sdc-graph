module RandomAttachment (runRandomAttachment) where

import Graph
import Data.List
import System.Random
import qualified Data.IntMap.Strict as Map

-- runRandomAttachment :: NrNodos -> Graph
runRandomAttachment :: Int -> IO ((Graph Int),Int)
runRandomAttachment 1 = return ((createGraph 1),0)
runRandomAttachment num =
  let
    graph = createGraph num
    in loop graph 0

loop :: (Graph Int) -> Int -> IO ((Graph Int),Int)
loop g edge_nr = do
  v1 <- randomRIO (0,(length $ Map.keys g)-1)
  v2 <- randomRIO (0,(length $ Map.keys g)-1)
  if v1==v2
  then loop g edge_nr
  else do
    let new_graph = addEdge g (v1,v2)
    if verifyConnected new_graph
      then return (new_graph,edge_nr+1)
      else loop new_graph (edge_nr+1)
