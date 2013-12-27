module Megahaskhal.Internal where

import Data.ByteString.Lazy (ByteString)
import Data.Sequence (Seq)
import qualified Data.Map.Strict as M

import Data.Word

type Dictionary = Seq ByteString

rawAuxWords = [
    "DISLIKE", "HE", "HER", "HERS", "HIM", "HIS", "I", "I'D", "I'LL", "I'M",
    "I'VE", "LIKE", "ME", "MY", "MYSELF", "ONE", "SHE", "THREE", "TWO", "YOU",
    "YOU'D", "YOU'LL", "YOU'RE", "YOU'VE", "YOUR", "YOURSELF"]
auxWords = M.fromList $ zip rawAuxWords $ repeat True

data Tree = Tree {
    getSymbol :: !Word16
    , getUsage :: !Word32
    , getCount :: !Word16
    , getChildren :: Seq Tree
    } deriving (Show)

data Brain = Brain {
    getForward :: Tree
    , getBackward :: Tree
    , getCookie :: String
    , getOrder :: Int
    , getDictionary :: Dictionary
} deriving (Show)

type Context = Seq Tree

isAuxWord :: String -> Bool
isAuxWord = flip M.member auxWords
