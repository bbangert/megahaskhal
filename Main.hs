module Main where

import Control.Monad (forever)
import Control.Monad.State (runState)
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, setStdGen)

import Megahaskhal (loadBrainFromFilename, reply)

main = do
    args <- getArgs
    if length args == 0
        then putStrLn "Pass in a file name for the brain."
        else let (filename:_) = args in runHal filename


runHal :: String -> IO ()
runHal filename = do
    result <- loadBrainFromFilename filename
    case result of
        Nothing -> putStrLn "Unable to load from file."
        Just brain -> forever $ do
            ranGen <- getStdGen
            putStrLn "Enter text: "
            input <- getLine
            let (output, newGen) = runState (reply brain input) ranGen
            putStrLn output
            setStdGen newGen
