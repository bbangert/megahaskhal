module Megahaskhal.Replies (
    ScoredReply (ScoredReply),
    sReply,
    sScore,
    TopReplies, empty, addReply, maxCapacity, curCapacity, allReplies,
    ) where

import qualified Data.List.Ordered as O
import           Data.Text         (Text)

data ScoredReply = ScoredReply { sReply :: Text
                               , sScore :: Float} deriving (Show)

instance Eq ScoredReply where
    x == y = sScore x == sScore y

instance Ord ScoredReply where
    x `compare` y = sScore x `compare` sScore y

data TopReplies = TopReplies { maxCapacity :: Int
                             , curCapacity :: Int
                             , allReplies  :: [ScoredReply]} deriving (Show)

-- | An empty TopReplies with a specified max capacity
empty :: Int -> TopReplies
empty x = TopReplies x 0 []

addReply :: ScoredReply -> TopReplies -> TopReplies
addReply s (TopReplies mC _ []) = TopReplies mC 1 [s]
addReply s t@(TopReplies mC cC replies@(h:rst))
    | any (\x -> sReply s == sReply x) replies = t
    | cC < mC   = TopReplies mC (cC+1) $ O.insertBag s replies
    | s > h     = t { allReplies = O.insertBag s rst }
    | otherwise = t
