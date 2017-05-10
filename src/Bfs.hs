module Bfs where

import Graph
import System.Random
import Data.Maybe
import Data.List
import System.Directory
import Data.Tree
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as Map

depth :: Tree Int -> Int
depth (Node _ []) = 1
depth (Node _ l) =  (+1) $ maximum $ map depth l

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
    go (Node x ts) = f x (map go ts)

treeAppend :: (Int,Int) -> Tree Int -> Tree Int
treeAppend (pai,filho) (Node n l)
  | pai == n = Node n ((Node filho []):l)
  | otherwise = Node n (map (treeAppend (pai,filho)) l)

isPresent :: Int -> (Tree Int) -> Bool
isPresent x t = x `elem` (flatten t)

treeEdgerize :: Tree Int -> [(Int,Int)]
treeEdgerize (Node n []) = []
treeEdgerize (Node n l) = 
    let 
        curr = map (\(Node s _) -> (n,s)) l
        sons = concat $ map treeEdgerize l
    in curr++sons

treeGraphVizRepresentation :: (Tree Int) -> String
treeGraphVizRepresentation t =
  let
      header = "digraph G {\n  nodesep=0.3;\n  ranksep=0.2;\n  margin=0.1;\n  node [shape=circle];\n"
      footer = "}\n"
      middle = concat $ map edgerize $ treeEdgerize t 
  in header++middle++footer
  where
    edgerize (l,r) = "  "++(show l)++" -> "++(show r)++";\n" 

printTree :: FilePath -> Tree Int -> IO ()
printTree dir t =  publishGraphVizRepr (treeGraphVizRepresentation t) dir >>= \x -> putStrLn $ "Printed tree on "++x++"!"

getDirName :: IO FilePath
getDirName = do
    resp <- doesDirectoryExist "result/"
    if resp then getDirAux 0 else return "result/"
  where getDirAux n = doesDirectoryExist ("result"++(show n)++"/") >>= \x -> if x then getDirAux (n+1) else return $ "result"++(show n)++"/"

runBfs :: (Int -> IO ((Graph Int),Int)) -> Int -> Int -> IO ()
runBfs f n start_node = do
    dirName <- getDirName
    createDirectory dirName
    (g,_) <- f n
    runBfs_aux dirName g [(-1,start_node)] Nothing

runBfs_aux :: FilePath -> (Graph Int) -> [(Int,Int)] -> Maybe (Tree Int) -> IO ()
runBfs_aux dirName _ [] (Just t) = printTree (dirName++"final_tree.png") t
runBfs_aux dirName g ((_,h):[]) Nothing =
    let
        tree = Node h []
        next_iter = map (\a -> (h,a)) $ Set.toList (maybe Set.empty id (Map.lookup h g))
        col = foldr (\x m -> Map.insert x True m) Map.empty (flatten tree)
        gvr = graphVizRepresentation g col
    in 
        do
            let fname = ("bfs_step_0.png")
            path <- publishGraphVizRepr gvr (dirName++fname) 
            putStrLn $ "Written graph "++fname++"!"
            runBfs_aux dirName g next_iter (Just tree)
runBfs_aux _ _ _ Nothing = putStrLn "Algorithm Error!"
runBfs_aux dirName g grey (Just t) =
    let
        new_tree = foldr (\x tr -> treeAppend x tr) t grey
        filtered_grey = map snd grey
        neighbours_grey = map (\x -> (,) x $ filter (\x -> not $ x `isPresent` new_tree) $ maybe [] id $ Set.toList `fmap` (Map.lookup x g)) filtered_grey
        next_iter = nubBy (\a b -> (snd a)==(snd b)) $ foldr (\(x,l) ac -> ac++(map (\y -> (x,y)) l)) [] neighbours_grey
        col = foldr (\x m -> Map.insert x True m) Map.empty (flatten new_tree)
        gvr = graphVizRepresentation g col
    in
        do
            let fname = ("bfs_step_"++(show (depth t))++".png")
            path <- publishGraphVizRepr gvr (dirName++fname) 
            putStrLn $ "Written graph "++fname++"!"
            runBfs_aux dirName g next_iter (Just new_tree)