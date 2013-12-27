module Megahaskhal.Internal where

import Data.ByteString.Lazy (ByteString)
import Data.IntMap.Strict (IntMap)


import Data.Word

type Dictionary = IntMap ByteString

data Tree = Tree {
    getSymbol :: !Word16
    , getUsage :: !Word32
    , getCount :: !Word16
    , getChildren :: [Tree]
    } deriving (Show)

data Brain = Brain {
    getForward :: Tree
    , getBackward :: Tree
    , getCookie :: String
    , getOrder :: Int
    , getDictionary :: Dictionary
} deriving (Show)
