module Main where

import Graph
import RandomAttachment
import System.Environment
import System.TimeIt
import System.IO
import Data.List
import Control.Monad

import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import Data.Colour.SRGB

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
     "random" -> shellRandom (tail w) 
     "preferential" -> return True
     "exit" -> return False
     _ -> printHelp >> return True

shellRandom :: [String] -> IO Bool
shellRandom (scl:(x:[])) = do
    let upper_bound = (read x :: Int)
    let scale = (read scl :: Int)
    let delta = list_scale scale upper_bound
    results <- mapM (avg_run_random 10) delta 
    let plot_data = map (\(pos,edges) -> (show pos,[edges])) $ zip delta results
    plotIt ("result_random_"++x++".png") plot_data
    return True
shellRandom _ = printHelp >> return True

printHelp = putStrLn "===Available Commands===\nrandom [scale] [upperbound]\npreferential [scale] [upperbound]\nhelp\nexit"

list_scale scale ub = takeWhile (<=ub) $ map (\x -> scale*x) [1..]

avg_run_random :: Int -> Int -> IO Double
avg_run_random times v = do
    results <- mapM (\_ -> (runRandomAttachment v)) [0..times]
    return $ average $ map snd results
  where average xs = realToFrac (sum xs) / genericLength xs

titles = ["Number of Nodes"]

plotIt filename values = toFile def filename $ do
    layout_title .= "Average Edges Needed till Connected Graph"
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
    plot $ fmap plotBars $ bars titles (addIndexes (map snd values))
