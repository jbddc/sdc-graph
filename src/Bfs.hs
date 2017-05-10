module Bfs where

import Graph
import RandomAttachment
import System.Random
import Data.Maybe
import qualified Data.Set as Set

data Tree a = Empty | Tree a (Tree a) (Tree a)
  deriving (Show, Eq)

runBfs :: Int -> Int -> IO ()
runBfs n start_node = do
    (g,_) <- runRandomAttachment n
    runBfs_aux g [] [start_node] (Tree start_node Empty Empty)

runBfs_aux _ _ [] t = printTree t
runBfs_aux g black grey tree = 
    let
        new_black = black++grey
        next_neighbours = map (\x -> maybe [] (Set.toList) $ Map.lookup x g) grey
        new_grey = nub $ filter (`elem` new_black) $ concat next_neighbours
    in
                

