{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Megahaskhal.Dictionary (
  Dictionary,
  addWord,
  addAllWords,
  findWord,
  lookupIndex,
  replicateM,
  forM_,
  emptyDictionary,
  length ) where

import           Control.DeepSeq    (NFData)
import qualified Data.Foldable      as F
import qualified Data.IntMap.Strict as I
import qualified Data.Map.Strict    as M
import           Data.Text          (Text, append)
import           Prelude            hiding (length)

data Dictionary = Dictionary
  { dWordId :: !(M.Map Text Int)
  , dIdWord :: !(I.IntMap Text)
  } deriving (Show)

instance NFData Dictionary

forM_ :: Monad m => Dictionary -> (Text -> m a) -> m ()
forM_ d m = mapM_ (m . findWord d) [0 .. length d]

blankDictionary :: Dictionary
blankDictionary = Dictionary M.empty I.empty

emptyDictionary :: Dictionary
emptyDictionary = addAllWords (Dictionary M.empty I.empty) defaultWords
  where defaultWords = ["<ERROR>", "<FIN>"]

findWord :: Dictionary -> Int -> Text
findWord = (I.!) . dIdWord

lookupIndex :: Text -> Dictionary -> Maybe Int
lookupIndex k = M.lookup k . dWordId

addWord :: Text -> Dictionary -> (Int, Dictionary)
addWord s orig@(Dictionary wordId idWord) =
  case M.lookup s wordId of
    Just ident -> (ident, orig)
    Nothing    -> (len, dict)
  where
    len  = M.size wordId
    dict = Dictionary (M.insert s len wordId) (I.insert len s idWord)

-- |For loading purposes, some ancient brains have bugs that caused duplicate
-- words to get loaded. Rather than mess that up, we force the addition of the
-- word by mangling it some more. In this case by adding spaces till it gets
-- in there.
forceAddWord :: Text -> Dictionary -> (Int, Dictionary)
forceAddWord s orig@(Dictionary wordId idWord) =
  case M.lookup s wordId of
    Just _ -> forceAddWord (append s " ") orig
    Nothing    -> (len, dict)
  where
    len  = M.size wordId
    dict = Dictionary (M.insert s len wordId) (I.insert len s idWord)

-- |Add all words to a dictionary if they weren't already in it, and return
-- the new dictionary
addAllWords :: Dictionary -> [Text] -> Dictionary
addAllWords = F.foldl' go
  where go d w = snd $ addWord w d

replicateM :: Monad m => Int -> m Text -> m Dictionary
replicateM reps m = go reps blankDictionary
  where
    go n !acc
      | n <= 0          = return acc
      | otherwise       = m >>= go (n - 1) . snd . (`forceAddWord` acc)

length :: Dictionary -> Int
length = M.size . dWordId
