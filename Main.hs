module Main where

import Control.Monad (forever)
import Control.Monad.State (runState)
import System.Environment (getArgs)
import System.Random (getStdGen, setStdGen)
import System.Exit (exitFailure)
import Megahaskhal (Brain, loadBrainFromFilename, reply, getWords)

die :: String -> IO ()
die s = putStrLn s >> exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
      [filename] ->
        loadBrainFromFilename filename >>=
          maybe (die "Unable to load from file.") runHal
      _ ->
        die "Pass in a file name for the brain."

runHal :: Brain -> IO ()
runHal brain = forever $ do
  ranGen <- getStdGen
  putStrLn "Enter text: "
  input <- getLine
  let phrase = getWords input
      (output, newGen) = runState (reply brain phrase) ranGen
  putStrLn output
  setStdGen newGen
