module Megahaskhal (
    loadBrainFromFilename,
    reply,
    Brain
    ) where

import Control.Monad.State (State, state)
import Data.Char (toTitle, toLower, toUpper)
import Data.List (concat, foldl1')
import Data.Maybe (catMaybes)
import Data.Sequence ( (|>), ViewR( (:>) ), ViewL( (:<) ))
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

reply :: I.Brain                        -- ^ A brain to start with
      -> String                         -- ^ Words to respond to
      -> State StdGen String            -- ^ Reply words
reply brain phrase = do
    let kw           = words . (Prelude.map toUpper) $! phrase
        dict         = I.getDictionary brain
        lookupSymbol = flip S.elemIndexL dict
        kws          = catMaybes $! Prelude.map lookupSymbol kw
        order        = I.getOrder brain
        ctx          = newContext (I.getForward brain) order
    symbol <- seed ctx dict kws
    (fWords, fSymbols, usedKey) <- processWords ctx dict order kws [] False symbol
    let backCtx      = newContext (I.getBackward brain) order
        minCtx       = min (Prelude.length fWords) order
        wordsToUse   = reverse $! Prelude.take minCtx fSymbols
        newBackCtx   = createBackContext backCtx order wordsToUse
    (bWords, bSymbols, _) <- autoBabble newBackCtx dict order kws fSymbols usedKey
    let lowered      = Prelude.map toLower $ foldl1' (++) $! (reverse bWords) ++ fWords
        titled       = toTitle (head lowered) : tail lowered
    return $! titled

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
