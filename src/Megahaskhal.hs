{-# LANGUAGE OverloadedStrings #-}
module Megahaskhal (
    loadBrainFromFilename,
    reply,
    getWords,
    Brain
    ) where

import Control.Monad.State (State, state)
import Data.Char (toUpper, isAlpha, isAlphaNum, isDigit)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import System.Random (randomR, StdGen, Random)
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Megahaskhal.Serialization (loadBrainFromFilename)
import Megahaskhal.Tree (
    Context, newContext, updateContext, createBackContext,
    Tree, lastTree,
    getCount, getChildren, getSymbol, getUsage,
    )
import qualified Megahaskhal.Internal as I
import Megahaskhal.Internal (Brain)

-- | Aliases for easy reading
type Keywords = [Int]
type Replies = [Int]
type UsedKey = Bool
type Symbol = Int
type Order = Int

-- Rules for tokenization:
-- Four character classes: alpha, digit, apostrophe, and other
-- If the character class changed from the previous to current character, then
-- it is a boundary. The only special case is alpha -> apostrophe -> alpha,
-- which is not considered to be a boundary (it's considered to be alpha).
-- If the last word is alphanumeric then add a last word of ".", otherwise
-- replace the last word with "." unless it already ends with one of "!.?".
getWords :: Text -> [Text]
getWords = fixup . T.groupBy sameClass . T.toUpper
  where
    firstAlpha = isAlpha . T.head
    -- find boundaries
    sameClass a b = isAlpha a == isAlpha b && isDigit a == isDigit b
    -- fix apostrophes
    fixup (a:b:c:rest)
      | firstAlpha a && b == "\'" && firstAlpha c =
        fixup (T.concat [a, b, c] : rest)
    -- fix the last word
    fixup (a:[])
      | isAlphaNum (T.head a) = [a, "."]
      | T.last a `elem` "!.?" = [a]
      | otherwise             = ["."]
    -- simple recursive case
    fixup (a:rest) = a : fixup rest
    -- handle empty input
    fixup [] = []

-- | Reply to a phrase with a given brain
reply :: I.Brain                        -- ^ A brain to start with
      -> [Text]                         -- ^ Words to respond to
      -> State StdGen Text              -- ^ Reply words
reply (I.Brain fTree bTree _ order dict) phrase = do
    let lookupSymbol = flip S.elemIndexL dict
        kws          = mapMaybe lookupSymbol phrase
        ctx          = newContext fTree order
    symbol <- seed ctx dict kws
    (fWords, fSymbols, usedKey) <- processWords ctx dict order kws [] False symbol
    let minCtx       = min (length fWords) order
        wordsToUse   = reverse $ take minCtx fSymbols
        backCtx      = createBackContext bTree order wordsToUse
    (bWords, _bSymbols, _) <- autoBabble backCtx dict order kws fSymbols usedKey
    let lowered      = T.toLower . T.concat $ reverse bWords ++ fWords
        titled = case T.uncons lowered of
          Just (c, t) -> toUpper c `T.cons` t
          _           -> lowered
    return titled

rndIndex :: Int -> State StdGen Int
rndIndex n = state $ randomR (0, n - 1)

-- | Seed a first word for the reply words
seed :: Context -> I.Dictionary -> [Int] -> State StdGen Int
seed ctx _dict []
    | childLength == 0  = return 0
    | otherwise         = do
        childIndex <- rndIndex childLength
        return $ getSymbol $ V.unsafeIndex children childIndex
    where children      = getChildren $ head ctx
          childLength   = V.length children
seed _ctx _dict keywords = do
    ind <- rndIndex (length keywords)
    return $ keywords !! ind

-- | Return a random word from the current context.
babble :: Context -> I.Dictionary -> Keywords -> Replies -> UsedKey
       -> State StdGen (Symbol, UsedKey)
babble ctx dict keywords replies used
    | childLength == 0  = return (0, used)
    | otherwise         = do
        position <- rndIndex childLength
        count <- rndIndex (1 + getUsage lastContext)
        return $ findWordToUse children dict keywords replies used 0 position count
    where lastContext = lastTree ctx
          children    = getChildren lastContext
          childLength = V.length children

-- | Find a word to use out of the vector of trees.
findWordToUse :: V.Vector Tree -> I.Dictionary -> Keywords -> Replies -> UsedKey -> Symbol
              -> Int                    -- ^ Current position
              -> Int                    -- ^ Remaining times to search
              -> (Symbol, UsedKey)
findWordToUse ctx dict keys replies used _symb pos count
  | symbol `elem` keys &&
    symbol `notElem` replies &&
    (used || not (I.isAuxWord $ S.index dict symbol)) =
      (symbol, True)
  | newCount < 0 = (symbol, used)
  | otherwise =
      findWordToUse ctx dict keys replies used symbol position newCount
  where
    node = V.unsafeIndex ctx pos
    symbol = getSymbol node
    newCount = count - getCount node
    position = case pos + 1 of
      p | p < V.length ctx -> p
        | otherwise        -> 0

-- | Babble with a given context.
autoBabble :: Context -> I.Dictionary -> Order -> Keywords -> Replies -> UsedKey
           -> State StdGen ([Text], [Int], UsedKey)
autoBabble ctx dict order keywords replies usedKey = do
    (symbol, usedKey') <- babble ctx dict keywords replies usedKey
    processWords ctx dict order keywords replies usedKey' symbol

processWords :: Context -> I.Dictionary -> Order -> Keywords -> Replies -> UsedKey -> Symbol
             -> State StdGen ([Text], [Int], UsedKey)
processWords _ _ _ _ _ uK 0 = return ([], [], uK)
processWords _ _ _ _ _ uK 1 = return ([], [], uK)
processWords ctx dict order keywords replies usedKey symbol = do
  (newSymbol, usedKey') <- babble newCtx dict keywords replyWords usedKey
  (rest, symbols, returnKey) <-
    processWords newCtx dict order keywords replyWords usedKey' newSymbol
  return (word:rest, symbol:symbols, returnKey)
  where
    word = S.index dict symbol
    replyWords = symbol:replies
    newCtx = updateContext ctx order symbol
