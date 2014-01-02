{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (forever)
import Control.Monad.State (runState)
import System.Environment (getArgs)
import System.Random (getStdGen, setStdGen)
import System.Exit (exitFailure)
import Megahaskhal (Brain, loadBrainFromFilename, reply, getWords)
import qualified Data.Text as T
import qualified Data.Text.IO as T

die :: T.Text -> IO ()
die s = T.putStrLn s >> exitFailure

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
  T.putStrLn "Enter text: "
  phrase <- getWords <$> T.getLine
  (output, newGen) <- runState (reply brain phrase) <$> getStdGen
  T.putStrLn output
  setStdGen newGen
