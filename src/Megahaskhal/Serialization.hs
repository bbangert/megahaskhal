module Megahaskhal.Serialization (
    loadBrainFromFilename,
    ) where

import Control.Monad (replicateM)
import System.IO  (withFile, IOMode( ReadMode ))
import Codec.Binary.UTF8.String (decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Binary.Get as G
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Control.Applicative ((<$>))

import Megahaskhal.Internal (Brain (Brain), Dictionary)
import Megahaskhal.Tree (Tree (Tree), getChildren)

loadBrainFromFilename :: String -> IO (Maybe Brain)
loadBrainFromFilename "" = return Nothing
loadBrainFromFilename fileName = withFile fileName ReadMode parseFile
  where
    parseFile handle =
      mkBrain =<< G.runGet parseModel <$> BL.hGetContents handle
    mkBrain (cookie, order, forward, backward, dictWords) = do
      print (cookie, order, leftCount, rightCount, S.length dictWords)
      return . Just $ Brain forward backward (LC.unpack cookie) order dictWords
      where
        leftCount = V.length $ getChildren forward
        rightCount = V.length $ getChildren backward

parseModel :: G.Get (BL.ByteString, Int, Tree, Tree, Dictionary)
parseModel = do
    cookie <- G.getLazyByteString 9
    order <- fromIntegral <$> G.getWord8
    leftTree <- parseTree
    rightTree <- parseTree
    dictLength <- fromIntegral <$> G.getWord32le
    dictWords <- S.replicateM dictLength parseWord
    return (cookie, order, leftTree, rightTree, dictWords)

parseTree :: G.Get Tree
parseTree = do
    symbol <- fromIntegral <$> G.getWord16le
    usage <- fromIntegral <$> G.getWord32le
    count <- fromIntegral <$> G.getWord16le
    branch <- fromIntegral <$> G.getWord16le
    children <- V.replicateM branch parseTree
    return $ Tree symbol usage count children

parseWord :: G.Get String
parseWord = do
    wordLength <- fromIntegral <$> G.getWord8
    rawWord <- replicateM wordLength G.getWord8
    return $ decode rawWord
