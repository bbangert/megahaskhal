module Megahaskhal (
    loadBrainFromFilename,
    seed,
    babble
    ) where

import Data.Word
import System.Random (getStdRandom, randomR)
import Megahaskhal.Serialization (loadBrainFromFilename)
import qualified Megahaskhal.Internal as I
import qualified Data.Sequence as S
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Sequence ( ViewR( (:>) ), ViewL( (:<) ))
import Data.Map.Strict as M


-- | Seed a first word for the reply words
seed :: I.Context -> IO Word16
seed context
    | childLength == 0 = return 0
    | otherwise = do
        childIndex <- getStdRandom $ randomR (0, childLength-1)
        return $ I.getSymbol . flip S.index childIndex $ children
    where children = I.getChildren $ S.index context 0
          childLength = S.length children


-- | Return a random word from the current context.
babble :: I.Context                 -- ^ Context
       -> I.Dictionary              -- ^ Dictionary of all the words
       -> [String]                  -- ^ List of keywords
       -> [String]                  -- ^ List of replies used
       -> Bool                      -- ^ Whether the key has been used
       -> IO (Word16, Bool)         -- ^ The symbol and whether the key was used
babble ctx dict keywords replies used
    | childLength == 0 = return (0, used)
    | otherwise = do
        position <- getStdRandom $ randomR (0, childLength-1)
        count <- getStdRandom $ randomR (0, (fromIntegral $ I.getUsage lastContext))
        let (front, end) = S.splitAt position children
            searchTree = S.take count $ end S.>< front
        return $ findWordToUse searchTree dict keywords replies used
    where _ :> lastContext = S.viewr ctx
          children = I.getChildren lastContext
          childLength = S.length children


-- | Find a word to use out of the sequence of trees. This sequence should be
-- appropriately chopped so that it represents exactly how many tree's
-- should be considered.
findWordToUse :: S.Seq I.Tree           -- ^ Sequence of trees to search
              -> I.Dictionary           -- ^ Dictionary of all words
              -> [String]               -- ^ List of keywords
              -> [String]               -- ^ List of replies used
              -> Bool                   -- ^ Whether the key has been used
              -> (Word16, Bool)         -- ^ The symbol and whether the key was used
findWordToUse ctx _ _ _ used
    | S.null ctx = (0, used)
findWordToUse ctx dict keys replies used
    | S.null remaining = (symbol, used)
    | and [elem word keys,
           or [used, not $ I.isAuxWord word],
           not $ elem word replies] = (symbol, True)
    | otherwise = findWordToUse remaining dict keys replies used
    where node :< remaining = S.viewl ctx
          symbol = I.getSymbol node
          word = LC.unpack $ S.index dict (fromIntegral symbol)
