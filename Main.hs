{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative   ((<$>))
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure)

import           Megahaskhal           (Brain, getWords, loadBrainFromFilename)
import           Megahaskhal.Reply     (generateReply)

die :: T.Text -> IO ()
die s = T.putStrLn s >> exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
      [filename] -> do
        loadBrainFromFilename filename >>=
          maybe (die "Unable to load from file.") runHal
      _ -> die "Pass in a file name for the brain."

runHal :: Brain -> IO ()
runHal brain = do
  T.putStrLn "Enter text: "
  phrase <- getWords <$> T.getLine
  print phrase
  reply <- generateReply brain phrase
  print reply
  runHal brain
