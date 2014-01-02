{-# LANGUAGE OverloadedStrings #-}
module Megahaskhal.Internal
       ( auxWords
       , isAuxWord
       , newBrainOrder
       , Brain(..)
       , Dictionary
       ) where

import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Set as S
import Megahaskhal.Tree (Tree)

type Dictionary = Seq Text

auxWords :: S.Set Text
auxWords =
  S.fromList [ "DISLIKE", "HE", "HER", "HERS", "HIM", "HIS", "I", "I'D"
             , "I'LL", "I'M", "I'VE", "LIKE", "ME", "MY", "MYSELF", "ONE"
             , "SHE", "THREE", "TWO", "YOU", "YOU'D", "YOU'LL", "YOU'RE"
             , "YOU'VE", "YOUR", "YOURSELF" ]

data Brain = Brain {
    getForward :: Tree
    , getBackward :: Tree
    , getCookie :: Text
    , getOrder :: Int
    , getDictionary :: Dictionary
} deriving (Show)

isAuxWord :: Text -> Bool
isAuxWord = (`S.member` auxWords)

newBrainOrder :: Brain -> Int -> Brain
newBrainOrder ob ord = ob { getOrder = ord }
