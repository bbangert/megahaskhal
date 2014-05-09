{-| Brain serialization and deserialization

-}

{-# LANGUAGE OverloadedStrings #-}

module Megahaskhal.Serialization (
  -- * Serialization
    saveBrainWithFilename

  -- * Deserialization
  , loadBrainFromFilename
  ) where

import           Control.Applicative      ((<$>), (<*>))
import qualified Data.Binary.Get          as G
import qualified Data.Binary.Put          as P
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Vector              as V
import           System.IO                (IOMode (ReadMode, WriteMode),
                                           withBinaryFile, withFile)

import qualified Megahaskhal.Dictionary   as D
import           Megahaskhal.Internal     (Brain (Brain))
import           Megahaskhal.Tree         (Tree (Tree), getChildren, mkTree)

w8, w16, w32 :: G.Get Int
w8 = fromIntegral <$> G.getWord8
w16 = fromIntegral <$> G.getWord16le
w32 = fromIntegral <$> G.getWord32le

lprefix :: Monad m => m Int -> (Int -> m a -> m b) -> m a -> m b
lprefix len rep action = len >>= flip rep action . fromIntegral

-- | Load a serialized brain from the filesystem
loadBrainFromFilename :: String -> IO (Maybe Brain)
loadBrainFromFilename "" = return Nothing
loadBrainFromFilename fileName = withFile fileName ReadMode parseFile
  where
    parseFile handle =
      mkBrain =<< G.runGet parseModel <$> BL.hGetContents handle
    mkBrain (cookie, order, forward, backward, dictWords) = do
      print (cookie, order, leftCount, rightCount, D.length dictWords)
      return . Just $ Brain forward backward cookie order dictWords
      where
        leftCount = V.length $ getChildren forward
        rightCount = V.length $ getChildren backward

parseModel :: G.Get (T.Text, Int, Tree, Tree, D.Dictionary)
parseModel =
  (,,,,) <$> parseText 9 -- cookie
         <*> w8          -- order
         <*> parseTree   -- leftTree
         <*> parseTree   -- rightTree
         <*> lprefix w32 D.replicateM (w8 >>= parseText) -- dictWords

parseTree :: G.Get Tree
parseTree = (return $!) =<<
  mkTree <$> G.getWord16le -- symbol
         <*> G.getWord32le -- usage
         <*> G.getWord16le -- count
         <*> lprefix w16 V.replicateM parseTree -- children

parseText :: Int -> G.Get T.Text
parseText n = (return $!) =<<
  T.decodeUtf8With T.lenientDecode <$> G.getByteString (fromIntegral n)

saveBrainWithFilename :: Brain -> String -> IO ()
saveBrainWithFilename brain fileName = withBinaryFile fileName WriteMode saveFile
  where saveFile handle = BL.hPut handle $ P.runPut $ saveBrain brain

saveBrain :: Brain -> P.Put
saveBrain (Brain ft bt _ order dict) = do
  P.putByteString "MegaHALv8"
  P.putWord8 $ fromIntegral order
  putTree ft
  putTree bt
  P.putWord32le $ fromIntegral $ D.length dict
  D.forM_ dict putText

putTree :: Tree -> P.Put
putTree (Tree symbol usage count children) = do
  P.putWord16le symbol
  P.putWord32le usage
  P.putWord16le count
  P.putWord16le $ fromIntegral $ V.length children
  V.forM_ children putTree

putText :: T.Text -> P.Put
putText txt =  do
  let wd = T.encodeUtf8 txt
      len = B.length wd
  P.putWord8 $ fromIntegral len
  P.putByteString wd
