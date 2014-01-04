module Megahaskhal.Dictionary (
  Dictionary,
  findWord,
  lookupIndex,
  replicateM,
  length ) where

import Prelude hiding (length)
import Data.Text (Text)
import qualified Data.Sequence as S

type Dictionary = S.Seq Text

findWord :: Dictionary -> Int -> Text
findWord = S.index

lookupIndex :: Text -> Dictionary -> Maybe Int
lookupIndex = S.elemIndexL

replicateM :: Monad m => Int -> m Text -> m Dictionary
replicateM = S.replicateM

length :: Dictionary -> Int
length = S.length
