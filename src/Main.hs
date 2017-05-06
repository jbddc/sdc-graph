module Main where

import Data.Graph.Generators.Regular
import Data.Graph.Generators.FGL
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Generators as Graphs

main = putStr "Number of Nodes: " >> readLn >>= startWorld 


startWorld size = do
    let initGraph = emptyGraph size
    print $ Graphs.edges initGraph

degree :: GraphInfo -> Int -> Int
degree graph node = sum 



