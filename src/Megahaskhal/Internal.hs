module Megahaskhal.Internal where

import Prelude hiding (null)
import Data.ByteString.Lazy (ByteString)
import Data.Sequence (Seq)
import System.Random (StdGen)
import qualified Data.Map.Strict as M

import Data.Word

type Dictionary = Seq String

rawAuxWords = [
    "DISLIKE", "HE", "HER", "HERS", "HIM", "HIS", "I", "I'D", "I'LL", "I'M",
    "I'VE", "LIKE", "ME", "MY", "MYSELF", "ONE", "SHE", "THREE", "TWO", "YOU",
    "YOU'D", "YOU'LL", "YOU'RE", "YOU'VE", "YOUR", "YOURSELF"]
auxWords = M.fromList $ zip rawAuxWords $ repeat True

data Tree = Empty | Tree {
    getSymbol :: !Int
    , getUsage :: !Int
    , getCount :: !Int
    , getChildren :: Seq Tree
    } deriving (Eq, Show)

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

null :: Tree -> Bool
null Empty = True
null _ = False

newBrainOrder :: Brain -> Int -> Brain
newBrainOrder ob ord = Brain (getForward ob) (getBackward ob) (getCookie ob) ord (getDictionary ob)
