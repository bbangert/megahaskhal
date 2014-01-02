{-# LANGUAGE OverloadedStrings #-}

module Megahaskhal (
    loadBrainFromFilename,
    reply,
    getWords,
    Brain
    ) where

import Control.Monad.State (State, state)
import Data.Char (toTitle, toLower, toUpper, isAlpha, isAlphaNum, isDigit)
import Data.List (concat, foldl1', splitAt)
import Data.Maybe (mapMaybe)
import System.Random (getStdRandom, randomR, StdGen, RandomGen, Random)
import qualified Data.Map.Strict as M
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
getWords :: T.Text -> [T.Text]
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
      -> [T.Text]                         -- ^ Words to respond to
      -> State StdGen T.Text              -- ^ Reply words
reply (I.Brain fTree bTree _ order dict) phrase = do
    let lookupSymbol = flip S.elemIndexL dict
        kws          = mapMaybe lookupSymbol phrase
        ctx          = newContext fTree order
    symbol <- seed ctx dict kws
    (fWords, fSymbols, usedKey) <- processWords ctx dict order kws [] False symbol
    let minCtx       = min (Prelude.length fWords) order
        wordsToUse   = reverse $ Prelude.take minCtx fSymbols
        backCtx      = createBackContext bTree order wordsToUse
    (bWords, bSymbols, _) <- autoBabble backCtx dict order kws fSymbols usedKey
    let lowered      =  T.toLower $ T.concat $ reverse bWords ++ fWords
        titled       = T.cons (toTitle $ T.head lowered) (T.tail lowered)
    return titled

-- | Seed a first word for the reply words
seed :: Context -> I.Dictionary -> [Int] -> State StdGen Int
seed ctx dict []
    | childLength == 0  = return 0
    | otherwise         = do
        childIndex <- state $ randomR (0, childLength-1)
        return $ getSymbol $ V.unsafeIndex children childIndex
    where children      = getChildren $ head ctx
          childLength   = V.length children
seed ctx dict keywords = do
    ind <- state $ randomR (0, Prelude.length keywords - 1)
    return $ keywords !! ind

-- | Return a random word from the current context.
babble :: Context -> I.Dictionary -> Keywords -> Replies -> UsedKey
       -> State StdGen (Symbol, UsedKey)
babble ctx dict keywords replies used
    | childLength == 0  = return (0, used)
    | otherwise         = do
        position <- state $ randomR (0, childLength-1)
        count <- state $ randomR (0, getUsage lastContext)
        return $ findWordToUse children dict keywords replies used 0 position count
    where lastContext = lastTree ctx
          children    = getChildren lastContext
          childLength = V.length children

-- | Find a word to use out of the vector of trees.
findWordToUse :: V.Vector Tree -> I.Dictionary -> Keywords -> Replies -> UsedKey -> Symbol
              -> Int                    -- ^ Current position
              -> Int                    -- ^ Remaining times to search
              -> (Symbol, UsedKey)
findWordToUse ctx dict keys replies used symb pos count
    | symbol `elem` keys && symbol `notElem` replies &&
        (used || not (I.isAuxWord $ S.index dict symbol)) = (symbol, True)
    | otherwise =
        let newCount = count - getCount node
        in if newCount < 0
            then (symbol, used)
            else
                let position = if pos+1 >= V.length ctx then 0 else pos+1
                in findWordToUse ctx dict keys replies used symbol position newCount
    where node   = V.unsafeIndex ctx pos
          symbol = getSymbol node

-- | Babble with a given context.
autoBabble :: Context -> I.Dictionary -> Order -> Keywords -> Replies -> UsedKey
           -> State StdGen ([T.Text], [Int], UsedKey)
autoBabble ctx dict order keywords replies usedKey = do
    (symbol, usedKey') <- babble ctx dict keywords replies usedKey
    processWords ctx dict order keywords replies usedKey' symbol

processWords :: Context -> I.Dictionary -> Order -> Keywords -> Replies -> UsedKey -> Symbol
             -> State StdGen ([T.Text], [Int], UsedKey)
processWords _ _ _ _ _ uK 0 = return ([], [], uK)
processWords _ _ _ _ _ uK 1 = return ([], [], uK)
processWords ctx dict order keywords replies usedKey symbol = do
    let word       = S.index dict symbol
        replyWords = symbol:replies
        newCtx     = updateContext ctx order symbol
    (newSymbol, usedKey') <- babble newCtx dict keywords replyWords usedKey
    (rest, symbols, returnKey) <-
        processWords newCtx dict order keywords replyWords usedKey' newSymbol
    return (word:rest, symbol:symbols, returnKey)
