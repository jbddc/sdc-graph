module Main where

import Graph
import RandomAttachment
import PrefAttachment
import System.Environment
import System.TimeIt
import System.IO
import Data.List
import Control.Monad

import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

main :: IO ()
main = do
    putStr "command: "
    hFlush stdout
    hSetBuffering stdin LineBuffering
    cmd <- getLine
    (time,result) <- timeItT $ runCmd cmd
    putStrLn $ "Command \'"++cmd++"\' took "++(show time)++" seconds."
    if result then main else return ()

runCmd :: String -> IO Bool
runCmd s = let
  w = words s
  in case w!!0 of
     "random" -> shellExec runRandomAttachment w 
     "pref" -> shellExec runPrefAttachment w
     "bfs"  -> bfsExec undefined w
     "exit" -> return False
     _ -> printHelp >> return True

bfsExec = undefined

shellExec :: (Int -> IO ((Graph Int),Int)) -> [String] -> IO Bool
shellExec cmd (cm:(scl:(x:[]))) = do
    let upper_bound = (read x :: Int)
    let scale = (read scl :: Int)
    let delta = list_scale scale upper_bound
    results <- mapM (avg_run cmd 10) delta 
    let plot_data = map (\(pos,edges) -> (show pos,[edges])) $ zip delta results
    plotIt ("res_avg_"++cm++"_"++x++".png") plot_data
    plot_data2 <- cmd upper_bound
    plotItDegree ("res_degree_"++cm++"_"++x++".png") $ map (\(x,y) -> (show x,[y])) $ sortBy myCompFunc $ degrees $ fst plot_data2
    return True
shellExec _ _ = printHelp >> return True

myCompFunc (x1,y1) (x2,y2) 
  | y1 < y2 = GT
  | y1 > y2 = LT
  | otherwise = EQ

printHelp = putStrLn "===Available Commands===\nrandom [scale] [upperbound]\npref [scale] [upperbound]\nhelp\nexit"

list_scale scale ub = takeWhile (<=ub) $ map (\x -> scale*x) [2..]

avg_run :: (Int -> IO ((Graph Int),Int)) -> Int -> Int -> IO Double
avg_run cmd times v = do
    results <- mapM (\_ -> (cmd v)) [0..times]
    return $ average $ map snd results
  where average xs = realToFrac (sum xs) / genericLength xs

titles = ["Number of Nodes"]
title = ["Degree"]

plotIt filename values = toFile def filename $ do
    layout_title .= "Average Edges Needed till Connected Graph"
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
    plot $ fmap plotBars $ bars titles (addIndexes (map snd values))

plotItDegree filename values = toFile def filename $ do
    layout_title .= "Degrees Per Node"
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
    plot $ fmap plotBars $ bars title (addIndexes (map snd values))