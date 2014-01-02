{-# LANGUAGE OverloadedStrings #-}
module Megahaskhal.Serialization (
    loadBrainFromFilename,
    ) where

import System.IO  (withFile, IOMode( ReadMode ))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Control.Applicative ((<$>), (<*>))

import Megahaskhal.Internal (Brain (Brain), Dictionary)
import Megahaskhal.Tree (Tree (Tree), getChildren)

w8, w16, w32 :: G.Get Int
w8 = fromIntegral <$> G.getWord8
w16 = fromIntegral <$> G.getWord16le
w32 = fromIntegral <$> G.getWord32le

lprefix :: Monad m => m Int -> (Int -> m a -> m b) -> m a -> m b
lprefix len rep action = flip rep action =<< len

loadBrainFromFilename :: String -> IO (Maybe Brain)
loadBrainFromFilename "" = return Nothing
loadBrainFromFilename fileName = withFile fileName ReadMode parseFile
  where
    parseFile handle =
      mkBrain =<< G.runGet parseModel <$> BL.hGetContents handle
    mkBrain (cookie, order, forward, backward, dictWords) = do
      print (cookie, order, leftCount, rightCount, S.length dictWords)
      return . Just $ Brain forward backward cookie order dictWords
      where
        leftCount = V.length $ getChildren forward
        rightCount = V.length $ getChildren backward

parseModel :: G.Get (T.Text, Int, Tree, Tree, Dictionary)
parseModel =
  (,,,,) <$> parseText 9 -- cookie
         <*> w8          -- order
         <*> parseTree   -- leftTree
         <*> parseTree   -- rightTree
         <*> lprefix w32 S.replicateM (w8 >>= parseText) -- dictWords

parseTree :: G.Get Tree
parseTree =
  Tree <$> w16 -- symbol
       <*> w32 -- usage
       <*> w16 -- count
       <*> lprefix w16 V.replicateM parseTree -- children

parseText :: Int -> G.Get T.Text
parseText n = do
  bs <- G.getByteString n
  return $! T.decodeUtf8With T.lenientDecode bs
