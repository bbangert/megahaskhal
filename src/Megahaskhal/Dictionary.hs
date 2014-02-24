module Megahaskhal.Dictionary (
  Dictionary,
  findWord,
  lookupIndex,
  replicateM,
  length ) where

import qualified Data.Sequence as S
import           Data.Text     (Text)
import           Prelude       hiding (length)

type Dictionary = S.Seq Text

findWord :: Dictionary -> Int -> Text
findWord = S.index

lookupIndex :: Text -> Dictionary -> Maybe Int
lookupIndex = S.elemIndexL

replicateM :: Monad m => Int -> m Text -> m Dictionary
replicateM = S.replicateM

length :: Dictionary -> Int
length = S.length
