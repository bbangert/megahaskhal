{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (foldl')
import Control.Applicative ((<$>))
import Control.Monad.State (runState)
import System.Environment (getArgs)
import System.Random (getStdGen, StdGen, mkStdGen)
import System.Exit (exitFailure)
import Megahaskhal (Brain, loadBrainFromFilename, craftReply, getWords)
import Megahaskhal.Replies (sReply, sScore)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.GetOpt (getOpt, ArgOrder(..), ArgDescr(..), OptDescr(..))

die :: T.Text -> IO ()
die s = T.putStrLn s >> exitFailure

data Flags = Flags { fGetStdGen :: IO StdGen }

defaultFlags :: Flags
defaultFlags = Flags { fGetStdGen = getStdGen }

options :: [OptDescr (Flags -> Flags)]
options = [ Option "s" ["seed"] (ReqArg updateSeed "SEED") "set prng SEED"
          ]
  where
    updateSeed d flags = case reads d of
      [(n, "")] -> flags { fGetStdGen = return (mkStdGen n) }
      -- ignore invalid seeds, might make sense to change this around to
      -- show errors
      _         -> flags

main :: IO ()
main = do
    (makeFlags, args, errs) <- getOpt Permute options <$> getArgs
    let flags = foldl' (flip ($)) defaultFlags makeFlags
    gen <- fGetStdGen flags
    case (args, errs) of
      ([filename], []) ->
        loadBrainFromFilename filename >>=
          maybe (die "Unable to load from file.") (`runHal` gen)
      _ -> do
        mapM_ putStrLn errs
        die "Pass in a file name for the brain."

runHal :: Brain -> StdGen -> IO ()
runHal brain gen = do
  T.putStrLn "Enter text: "
  phrase <- getWords <$> T.getLine
  let (reply, newGen) = runState (craftReply brain phrase) gen
  T.putStrLn $ sReply reply
  print $ sScore reply
  runHal brain newGen
