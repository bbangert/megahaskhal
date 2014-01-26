{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (foldl')
import Data.Maybe (fromJust, isJust)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.State (runState)
import System.Environment (getArgs)
import System.Random (getStdGen, StdGen, mkStdGen, setStdGen)
import System.Exit (exitFailure)
import Megahaskhal (Brain, loadBrainFromFilename, customCraft, getWords)
import Megahaskhal.Replies (sReply, sScore)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.SimpleIRC as SI
import qualified Data.ByteString.Char8 as B
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

prefix = "|" -- TODO: Move this to the configuration file/Types.hs

startsWithPrefix :: B.ByteString -> B.ByteString -> Maybe B.ByteString
startsWithPrefix myNick m =
    if prefixedCmd
      then Just prefix
      else directCmd
  where
    prefixedCmd = (prefix) `B.isPrefixOf` m
    directCmd
      | (myNick `B.append` " ") `B.isPrefixOf` m =
          Just (myNick `B.append` " ")
      | (myNick `B.append` ": ") `B.isPrefixOf` m =
          Just (myNick `B.append` ": ")
      | (myNick `B.append` ", ") `B.isPrefixOf` m =
          Just (myNick `B.append` ", ")
      | (myNick `B.append` ":") `B.isPrefixOf` m =
          Just (myNick `B.append` ":")
      | (myNick `B.append` ",") `B.isPrefixOf` m =
          Just (myNick `B.append` ",")
      | otherwise = Nothing

-- |Checks if the message is a Private Message.
isPM :: SI.MIrc -> SI.IrcMessage -> IO Bool
isPM s m = do
  nick <- SI.getNickname s
  return $ nick == (fromJust $ SI.mChan m)

dropPrefix :: SI.IrcMessage -> B.ByteString -> B.ByteString
dropPrefix m prfx = B.drop (B.length prfx) (SI.mMsg m)

blabber :: Brain -> SI.EventFunc
blabber brain s m = do
  myNick <- SI.getNickname s
  let isPrfx = startsWithPrefix myNick (SI.mMsg m)

  isPrivMsg <- isPM s m

  let maybePrfx =
        if isJust isPrfx
          then isPrfx
          else if isPrivMsg then Just "" else Nothing
  when (isJust maybePrfx) $ do
    let prfx = fromJust maybePrfx
        words = T.pack $ B.unpack $ dropPrefix m prfx
    reply <- runHal brain words
    SI.sendMsg s chan $ B.pack . T.unpack $ reply
  where
    chan = fromJust $ SI.mChan m

main :: IO ()
main = do
  (makeFlags, args, errs) <- getOpt Permute options <$> getArgs
  let flags = foldl' (flip ($)) defaultFlags makeFlags

  -- All file loading(that is in elysia's directory) has to be done before calling
  -- daemonize, because it changes the current working dir.
  case (args, errs) of
    ([filename], []) -> do
      result <- loadBrainFromFilename filename
      case result of
        Just brain -> startBot brain
        Nothing    -> die "Unable to read the file"
    _ -> do
      mapM_ putStrLn errs
      die "Pass in a file name for the brain."

startBot brain = do
  let events = [(SI.Privmsg (blabber brain))]
      groovie = (SI.mkDefaultConfig "localhost" "benTogo")
              {
                SI.cPort = 26665
              , SI.cChannels = ["#kgb"] -- Channels to join on connect
              , SI.cEvents = events -- Events to bind
              }
  SI.connect groovie False True
  return ()

runHal :: Brain -> T.Text -> IO T.Text
runHal brain phrase = do
  gen <- getStdGen
  let words = getWords phrase
      (reply, newGen) = runState (customCraft (50, 12800) brain words) gen
  setStdGen newGen
  return $ sReply reply
