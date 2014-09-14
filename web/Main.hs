module Main where

import           System.Environment        (getArgs)

import           Megahaskhal.Learning      (learnFile)
import           Megahaskhal.Serialization (loadBrainFromFilename,
                                            saveBrainWithFilename)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [iBrain, trainer, oBrain] -> do
      Just brain <- loadBrainFromFilename iBrain
      newb <- learnFile brain trainer
      saveBrainWithFilename newb oBrain
    _ -> print "Error, you suck"
