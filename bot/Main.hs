{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative   ((<$>))
import           Control.Monad         (when)
import           Control.Monad.State   (runState)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe            (fromJust, isJust)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Megahaskhal           (Brain, customCraft, getWords,
                                        loadBrainFromFilename)
import           Megahaskhal.Replies   (sReply)
import qualified Network.SimpleIRC     as SI
import           System.Console.GetOpt (ArgDescr (..), ArgOrder (..),
                                        OptDescr (..), getOpt)
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure)
import           System.Random         (StdGen, getStdGen, mkStdGen, setStdGen)

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

prefix :: B.ByteString
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

onMessage :: Brain -> SI.EventFunc
onMessage brain s m = do
  myNick <- SI.getNickname s
  let isPrfx = startsWithPrefix myNick (SI.mMsg m)

  isPrivMsg <- isPM s m

  let maybePrfx =
        if isJust isPrfx
          then isPrfx
          else if isPrivMsg then Just "" else Nothing
  when (isJust maybePrfx) $ do
    let prfx = fromJust maybePrfx
        phrase = T.pack $ B.unpack $ dropPrefix m prfx
    reply <- runHal brain phrase
    SI.sendMsg s origin $ B.pack . T.unpack $ reply
  where
    origin = fromJust $ SI.mOrigin m

main :: IO ()
main = do
  (_, args, errs) <- getOpt Permute options <$> getArgs
  -- let flags = foldl' (flip ($)) defaultFlags makeFlags

  case (args, errs) of
    ([filename], []) -> loadBrainFromFilename filename >>=
        maybe (die "Unable to read the file") startBot
    _ -> do
      mapM_ putStrLn errs
      die "Pass in a file name for the brain."

startBot :: Brain -> IO ()
startBot brain = do
  let events = [(SI.Privmsg (onMessage brain))]
      groovie = (SI.mkDefaultConfig "localhost" "brotogo")
              {
                SI.cPort = 26665
              , SI.cChannels = ["#kgb", "#collo"] -- Channels to join on connect
              , SI.cEvents = events -- Events to bind
              }
  SI.connect groovie False True
  return ()

runHal :: Brain -> T.Text -> IO T.Text
runHal brain phrase = do
  gen <- getStdGen
  let (reply, newGen) = runState (customCraft (25, 5000) brain $ getWords phrase) gen
  setStdGen newGen
  return $ sReply reply
