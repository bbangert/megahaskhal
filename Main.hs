module Main where

import Control.Monad (forever)
import Control.Monad.State (runState)
import Data.Text (pack)
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, setStdGen)

import Megahaskhal (loadBrainFromFilename, reply, getWords)

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
            let phrase = getWords $ pack input
                (output, newGen) = runState (reply brain phrase) ranGen
            print output
            setStdGen newGen
