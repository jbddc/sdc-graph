module PrefAttachment (runPrefAttachment) where

import Graph
import Data.List
import System.Random
import Test.QuickCheck
import qualified Data.IntMap.Strict as Map

runPrefAttachment :: Int -> IO ((Graph Int),Int)
runPrefAttachment 1 = return ((createGraph 1),0)
runPrefAttachment num =
  let
    graph = createGraph num
    in loop graph 0

loop :: (Graph Int) -> Int -> IO ((Graph Int),Int)
loop g edge_nr = do
  v1 <- randomRIO (0,(length $ Map.keys g)-1)
  v2 <- generate $ frequency $ map (\(a,b) -> (b,return a)) $ degrees g
  if v1==v2
  then loop g edge_nr
  else do
    let new_graph = addEdge g (v1,v2)
    if verifyConnected new_graph
      then return (new_graph,edge_nr+1)
      else loop new_graph (edge_nr+1)
