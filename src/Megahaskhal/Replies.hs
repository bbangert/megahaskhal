module Megahaskhal.Replies (
    ScoredReply (ScoredReply),
    sReply,
    sScore,
    TopReplies, emptyReplies, addReply, curCapacity, allReplies,
    ) where

import Data.Text (Text)
import qualified Data.List.Ordered as O

data ScoredReply = ScoredReply { sReply :: Text
                               , sScore :: Float} deriving (Show)

instance Eq ScoredReply where
    x == y = (sScore x) == (sScore y)

instance Ord ScoredReply where
    x `compare` y = sScore x `compare` sScore y

data TopReplies = TopReplies { maxCapacity :: Int
                             , curCapacity :: Int
                             , allReplies  :: [ScoredReply]}

emptyReplies :: Int -> TopReplies
emptyReplies x = TopReplies x 0 []

addReply :: ScoredReply -> TopReplies -> TopReplies
addReply s (TopReplies mC _ []) = TopReplies mC 1 [s]
addReply s t@(TopReplies mC cC replies)
    | cC < mC          = t { allReplies = O.insertBag s replies, curCapacity = cC + 1 }
    | s > head replies =
            let (_:rst) = replies
            in t { allReplies = O.insertBag s replies }
    | otherwise        = t
