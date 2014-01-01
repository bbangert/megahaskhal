module Megahaskhal (
    loadBrainFromFilename,
    reply,
    getWords,
    Brain
    ) where

import Control.Monad.State (State, state)
import Data.Char (toTitle, toLower, toUpper, isAlpha, isAlphaNum, isDigit)
import Data.List (concat, foldl1', splitAt)
import Data.Maybe (catMaybes)
import System.Random (getStdRandom, randomR, StdGen, RandomGen, Random)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
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

-- | Split a string of words into a proper word-set for the reply
getWords :: String -> [String]
getWords "" = []
getWords s
    | isAlphaNum $! head lastWord       = phrase ++ ["."]
    | not $ last lastWord `elem` "!.?"  = init phrase ++ ["."]
    | otherwise                         = phrase
    where phrase = Prelude.map V.toList . _getWords 0 $! V.fromList $ Prelude.map toUpper s
          lastWord = last phrase

_getWords :: Int -> V.Vector Char -> [V.Vector Char]
_getWords offset phrase
    | isBoundary == True =
        let (word, newPhrase) = V.splitAt offset phrase
        in if V.length newPhrase == 0 then [word] else word:_getWords 0 newPhrase
    | otherwise          = _getWords (offset+1) phrase
    where isBoundary = boundary offset phrase

boundary :: Int -> V.Vector Char -> Bool
boundary 0 _                                    = False
boundary position string
    | position == stLen                         = True
    where stLen = V.length string
boundary position string
    | and [string V.! position == '\'',
           isAlpha $ string V.! (position-1),
           position+1 < V.length string,
           isAlpha $ string V.! (position+1)]   = False
boundary position string
    | and [position > 1, curIsAlpha,
           priorChar == '\'',
           isAlpha $ string V.! (position-2)]   = False
    | and [curIsAlpha, not priorAlpha]          = True
    | and [not curIsAlpha, priorAlpha]          = True
    where curIsAlpha = isAlpha $ string V.! position
          priorChar = string V.! (position-1)
          priorAlpha = isAlpha priorChar
boundary position string
    | not $ curIsDigit == priorIsDigit          = True
    | otherwise                                 = False
    where curIsDigit = isDigit $ string V.! position
          priorIsDigit = isDigit $ string V.! (position-1)

-- | Reply to a phrase with a given brain
reply :: I.Brain                        -- ^ A brain to start with
      -> [String]                         -- ^ Words to respond to
      -> State StdGen String            -- ^ Reply words
reply (I.Brain fTree bTree _ order dict) phrase = do
    let lookupSymbol = flip S.elemIndexL dict
        kws          = catMaybes $! Prelude.map lookupSymbol phrase
        ctx          = newContext fTree order
    symbol <- seed ctx dict kws
    (fWords, fSymbols, usedKey) <- processWords ctx dict order kws [] False symbol
    let minCtx       = min (Prelude.length fWords) order
        wordsToUse   = reverse $! Prelude.take minCtx fSymbols
        backCtx      = createBackContext bTree order wordsToUse
    (bWords, bSymbols, _) <- autoBabble backCtx dict order kws fSymbols usedKey
    let lowered      = Prelude.map toLower $ foldl1' (++) $! (reverse bWords) ++ fWords
        titled       = toTitle (head lowered) : tail lowered
    return $! titled

-- | Seed a first word for the reply words
seed :: Context -> I.Dictionary -> [Int] -> State StdGen Int
seed ctx dict []
    | childLength == 0  = return 0
    | otherwise         = do
        childIndex <- state $ randomR (0, childLength-1)
        return $! getSymbol $ V.unsafeIndex children childIndex
    where children      = getChildren $ head ctx
          childLength   = V.length children
seed ctx dict keywords = do
    ind <- state $ randomR (0, Prelude.length keywords - 1)
    return $! keywords !! ind

-- | Return a random word from the current context.
babble :: Context -> I.Dictionary -> Keywords -> Replies -> UsedKey
       -> State StdGen (Symbol, UsedKey)
babble ctx dict keywords replies used
    | childLength == 0  = return (0, used)
    | otherwise         = do
        position <- state $ randomR (0, childLength-1)
        count <- state $ randomR (0, getUsage lastContext)
        return $! findWordToUse children dict keywords replies used 0 position count
    where lastContext = lastTree ctx
          children    = getChildren lastContext
          childLength = V.length children

-- | Find a word to use out of the vector of trees.
findWordToUse :: V.Vector Tree -> I.Dictionary -> Keywords -> Replies -> UsedKey -> Symbol
              -> Int                    -- ^ Current position
              -> Int                    -- ^ Remaining times to search
              -> (Symbol, UsedKey)
findWordToUse ctx dict keys replies used symb pos count
    | and [elem symbol keys,
           not $ elem symbol replies,
           or [used, not $ I.isAuxWord $ S.index dict symbol]] = (symbol, True)
    | otherwise =
        let newCount = count - getCount node
        in if newCount < 0
            then (symbol, used)
            else
                let position = if pos+1 >= (V.length ctx) then 0 else pos+1
                in findWordToUse ctx dict keys replies used symbol position newCount
    where node   = V.unsafeIndex ctx pos
          symbol = getSymbol node

-- | Babble with a given context.
autoBabble :: Context -> I.Dictionary -> Order -> Keywords -> Replies -> UsedKey
           -> State StdGen ([String], [Int], UsedKey)
autoBabble ctx dict order keywords replies usedKey = do
    (symbol, usedKey') <- babble ctx dict keywords replies usedKey
    processWords ctx dict order keywords replies usedKey' symbol

processWords :: Context -> I.Dictionary -> Order -> Keywords -> Replies -> UsedKey -> Symbol
             -> State StdGen ([String], [Int], UsedKey)
processWords _ _ _ _ _ uK 0 = return ([], [], uK)
processWords _ _ _ _ _ uK 1 = return ([], [], uK)
processWords ctx dict order keywords replies usedKey symbol = do
    let word       = S.index dict symbol
        replyWords = symbol:replies
        newCtx     = updateContext ctx order symbol
    (newSymbol, usedKey') <- babble newCtx dict keywords replyWords usedKey
    (rest, symbols, returnKey) <-
        processWords newCtx dict order keywords replyWords usedKey' newSymbol
    return $! (word:rest, symbol:symbols, returnKey)
