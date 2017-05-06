module RandomAttachment (runRandomAttachment) where

import Graph
import Data.List
import System.Random
import qualified Data.IntMap.Strict as Map

-- runRandomAttachment :: NrNodos -> Graph
runRandomAttachment :: Int -> IO (Graph Int)
runRandomAttachment num =
  let
    graph = createGraph num
    in loop graph

loop :: (Graph Int) -> IO (Graph Int)
loop g = do
  v1 <- randomRIO (0,(toEnum $ length $ Map.keys g)-1)
  v2 <- randomRIO (0,(toEnum $ length $ Map.keys g)-1)
  if v1==v2
  then loop g
  else do
    let new_graph = addEdge g (v1,v2)
    if verifyConnected new_graph
      then return new_graph
      else loop new_graph
