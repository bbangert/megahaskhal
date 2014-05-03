{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad         (when)
import           Control.Monad.Error   (join, liftIO, runErrorT)
import qualified Data.ByteString.Char8 as B
import           Data.ConfigFile       (CPError, emptyCP, get, readfile)
import           Data.List.Split       (splitOneOf)
import           Data.Maybe            (fromJust, isJust)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Network.SimpleIRC     as SI
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure)
import           System.Random         (StdGen)

import           Megahaskhal           (Brain, getWords, loadBrainFromFilename)
import           Megahaskhal.Reply     (generateReply)

die :: T.Text -> IO ()
die s = T.putStrLn s >> exitFailure

data Flags = Flags { fGetStdGen :: IO StdGen }

data Settings = Settings { ircServer   :: String
                         , ircPort     :: Int
                         , ircNick     :: String
                         , ircChannels :: [String]
                         } deriving (Show)

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
  args <- getArgs
  case args of
    [brainFile, configFile] -> do
      results <- loadConfig brainFile configFile
      case results of
        Left errs -> ioError $ userError $ "Error loading megabot: " ++ errs
        Right (brain, settings) -> startBot brain settings
    _ -> ioError $ userError "Usage: megabot BRAINFILE CONFIGFILE"

loadConfig :: String -> String -> IO (Either String (Brain, Settings))
loadConfig brainFile configFile = loadBrainFromFilename brainFile >>= loadResult
  where
    loadResult Nothing      = return $ Left "Unable to load brain file"
    loadResult (Just brain) = parseConfigFile configFile >>= parseResult brain
    parseResult _ (Left (_, errs)) = return $ Left $ "Unable to parse config: " ++ errs
    parseResult brain (Right settings) = return $ Right (brain, settings)

startBot :: Brain -> Settings -> IO ()
startBot brain settings = do
  let events = [(SI.Privmsg (onMessage brain))]
      groovie = (SI.mkDefaultConfig (ircServer settings) $ ircNick settings)
              {
                SI.cPort = ircPort settings
              , SI.cChannels = ircChannels settings -- Channels to join on connect
              , SI.cEvents = events -- Events to bind
              }
  SI.connect groovie False True
  return ()

runHal :: Brain -> T.Text -> IO T.Text
runHal brain phrase = generateReply brain (getWords phrase)

parseConfigFile :: String
                -> IO (Either CPError Settings)
parseConfigFile filename = runErrorT $ do
  cp <- join $ liftIO $ readfile emptyCP filename
  server <- get cp "DEFAULT" "server"
  port <- get cp "DEFAULT" "port"
  nick <- get cp "DEFAULT" "nick"
  chans <- get cp "DEFAULT" "channels"
  let channels = filter (/= "") $ splitOneOf ", " $ chans
  return $ Settings server port nick channels
