module Megahaskhal.Serialization (
    loadBrainFromFilename,
    ) where

import Control.Monad (replicateM)
import Data.Word
import System.IO  (withFile, IOMode( ReadMode ))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Binary.Get as G
import qualified Data.Sequence as S

import Megahaskhal.Internal (Brain (Brain), Tree (Tree), Dictionary, getChildren)

loadBrainFromFilename :: String -> IO (Maybe Brain)
loadBrainFromFilename "" = return Nothing
loadBrainFromFilename fileName =
    withFile fileName ReadMode (\handle -> do
        contents <- BL.hGetContents handle
        let (cookie, order, forward, backward, dictWords) = G.runGet parseModel contents
            leftCount = S.length $ getChildren forward
            rightCount = S.length $ getChildren backward
        print (cookie, order, leftCount, rightCount, S.length dictWords)
        return $ Just $ Brain forward backward (LC.unpack cookie) (fromIntegral order) dictWords
    )

parseModel :: G.Get (BL.ByteString, Word8, Tree, Tree, Dictionary)
parseModel = do
    cookie <- G.getLazyByteString 9
    order <- G.getWord8
    leftTree <- parseTree
    rightTree <- parseTree
    dictLength <- G.getWord32le
    dictWords <- S.replicateM (fromIntegral dictLength) parseWord
    return (cookie, order, leftTree, rightTree, dictWords)

parseTree :: G.Get Tree
parseTree = do
    symbol <- G.getWord16le
    usage <- G.getWord32le
    count <- G.getWord16le
    branch <- G.getWord16le
    children <- S.replicateM (fromIntegral branch) parseTree
    return $ Tree (fromIntegral symbol :: Int) (fromIntegral usage :: Int) (fromIntegral count :: Int) children

parseWord :: G.Get String
parseWord = do
    wordLength <- G.getWord8
    word <- G.getLazyByteString $ fromIntegral wordLength
    return $ LC.unpack word