module Main where

import Graph
import RandomAttachment
import System.Environment

main :: IO ()
main = do
    arguments <- getArgs
    let number       = read (head arguments) :: Int
    g <- runRandomAttachment number
    print g
