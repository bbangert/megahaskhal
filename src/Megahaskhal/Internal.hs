module Megahaskhal.Internal where

import Data.ByteString.Lazy (ByteString)
import Data.Sequence (Seq)

import Data.Word

type Dictionary = Seq ByteString

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
