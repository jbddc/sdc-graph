module Graph where

import Control.Monad.Random
import System.Environment

type Graph = [(Integer, Integer, [Integer])]

createGraph :: Integer -> Graph
createGraph number = [(number', 1, []) | number' <- [1 .. number]]

weights :: Graph -> [(Integer, Rational)]
weights graph = [(x, toRational y) | (x, y, _) <- graph]

randomVertex :: RandomGen g => g -> Graph -> Integer
randomVertex generator graph = evalRand value generator
    where value = fromList . weights $ graph

addEdge :: Integer -> Integer -> Graph -> Graph
addEdge v1 v2 [(x, y, z)]       = [(x, y + 1, v2 : z)]
addEdge v1 v2 ((x, y, z) : xyz) = 
    if (v1 == x) 
    then (x, y + 1, v2 : z) : xyz 
    else (x, y, z) : addEdge v1 v2 xyz

main :: IO ()
main = do
    arguments <- getArgs
    let number       = read (head arguments) :: Integer
        initialGraph = createGraph number
    print initialGraph
    let vertexOne = randomVertex (mkStdGen 13579) initialGraph
        vertexTwo = randomVertex (mkStdGen 24680) initialGraph
        temporary = addEdge vertexOne vertexTwo initialGraph
        graph     = addEdge vertexTwo vertexOne temporary
    print graph
