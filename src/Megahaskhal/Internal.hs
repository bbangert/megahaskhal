{-# LANGUAGE OverloadedStrings #-}
module Megahaskhal.Internal
       ( rawAuxWords
       , auxWords
       , isAuxWord
       , newBrainOrder
       , Brain(..)
       , Dictionary
       ) where

import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Megahaskhal.Tree (Tree)

type Dictionary = Seq Text

rawAuxWords :: [Text]
rawAuxWords = [
    "DISLIKE", "HE", "HER", "HERS", "HIM", "HIS", "I", "I'D", "I'LL", "I'M",
    "I'VE", "LIKE", "ME", "MY", "MYSELF", "ONE", "SHE", "THREE", "TWO", "YOU",
    "YOU'D", "YOU'LL", "YOU'RE", "YOU'VE", "YOUR", "YOURSELF"]

auxWords :: M.Map Text Bool
auxWords = M.fromList $ zip rawAuxWords $ repeat True

data Brain = Brain {
    getForward :: Tree
    , getBackward :: Tree
    , getCookie :: Text
    , getOrder :: Int
    , getDictionary :: Dictionary
} deriving (Show)

isAuxWord :: Text -> Bool
isAuxWord = (`M.member` auxWords)

newBrainOrder :: Brain -> Int -> Brain
newBrainOrder ob ord = ob { getOrder = ord }
