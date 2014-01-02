module Megahaskhal.Internal where

import Data.ByteString.Lazy (ByteString)
import Data.Sequence (Seq)
import System.Random (StdGen)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T
import Megahaskhal.Tree (Tree)

type Dictionary = Seq T.Text

rawAuxWords = [
    "DISLIKE", "HE", "HER", "HERS", "HIM", "HIS", "I", "I'D", "I'LL", "I'M",
    "I'VE", "LIKE", "ME", "MY", "MYSELF", "ONE", "SHE", "THREE", "TWO", "YOU",
    "YOU'D", "YOU'LL", "YOU'RE", "YOU'VE", "YOUR", "YOURSELF"]
auxWords = M.fromList $ zip (map T.pack rawAuxWords) $ repeat True

data Brain = Brain {
    getForward :: Tree
    , getBackward :: Tree
    , getCookie :: String
    , getOrder :: Int
    , getDictionary :: Dictionary
} deriving (Show)

isAuxWord :: T.Text -> Bool
isAuxWord = flip M.member auxWords

newBrainOrder :: Brain -> Int -> Brain
newBrainOrder ob ord =
    Brain (getForward ob) (getBackward ob) (getCookie ob) ord (getDictionary ob)

wordCmp :: String -> String -> Int
wordCmp x y
    | x == y = 0
    | length x < length y = -1
    | otherwise = 1
