{-# LANGUAGE OverloadedStrings #-}

module Megahaskhal.Dictionary (
  Dictionary,
  addWord,
  findWord,
  lookupIndex,
  replicateM,
  forM_,
  emptyDictionary,
  length ) where

import           Data.Foldable (forM_)
import           Data.Sequence ((|>))
import qualified Data.Sequence as S
import           Data.Text     (Text)
import           Prelude       hiding (length)

type Dictionary = S.Seq Text

emptyDictionary :: Dictionary
emptyDictionary = S.fromList ["<ERROR>", "<FIN>"]

findWord :: Dictionary -> Int -> Text
findWord = S.index

lookupIndex :: Text -> Dictionary -> Maybe Int
lookupIndex = S.elemIndexL

addWord :: Text -> Dictionary -> (Int, Dictionary)
addWord s dict =
  case existingWord of
    Nothing -> (S.length dict, dict |> s)
    Just x -> (x, dict)
  where existingWord = S.findIndexL (==s) dict

replicateM :: Monad m => Int -> m Text -> m Dictionary
replicateM = S.replicateM

length :: Dictionary -> Int
length = S.length
