module Megahaskhal.Internal
       ( rawAuxWords
       , auxWords
       , isAuxWord
       , newBrainOrder
       , wordCmp
       , Brain(..)
       , Dictionary
       ) where

import Data.Sequence (Seq)
import qualified Data.Map.Strict as M
import Megahaskhal.Tree (Tree)

type Dictionary = Seq String

rawAuxWords :: [String]
rawAuxWords = [
    "DISLIKE", "HE", "HER", "HERS", "HIM", "HIS", "I", "I'D", "I'LL", "I'M",
    "I'VE", "LIKE", "ME", "MY", "MYSELF", "ONE", "SHE", "THREE", "TWO", "YOU",
    "YOU'D", "YOU'LL", "YOU'RE", "YOU'VE", "YOUR", "YOURSELF"]

auxWords :: M.Map String Bool
auxWords = M.fromList $ zip rawAuxWords $ repeat True

data Brain = Brain {
    getForward :: Tree
    , getBackward :: Tree
    , getCookie :: String
    , getOrder :: Int
    , getDictionary :: Dictionary
} deriving (Show)

isAuxWord :: String -> Bool
isAuxWord = (`M.member` auxWords)

newBrainOrder :: Brain -> Int -> Brain
newBrainOrder ob ord = ob { getOrder = ord }

wordCmp :: String -> String -> Int
wordCmp x y
    | x == y = 0
    | length x < length y = -1
    | otherwise = 1
