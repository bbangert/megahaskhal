module Megahaskhal (
    loadBrainFromFilename,
    reply,
    seed,
    babble,
    Brain
    ) where

import Control.Monad.State (State, state)
import Data.Char (toTitle, toLower, toUpper)
import Data.List (concat)
import Data.Maybe (catMaybes)
import Data.Sequence ( (|>), ViewR( (:>) ), ViewL( (:<) ))
import System.Random (getStdRandom, randomR, StdGen, RandomGen, Random)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Vector as V

import Megahaskhal.Serialization (loadBrainFromFilename)
import Megahaskhal.Tree (
    Context,
    Tree,
    newContext,
    lastTree,
    getCount,
    getChildren,
    getSymbol,
    getUsage,
    updateContext,
    createBackContext)
import qualified Megahaskhal.Internal as I
import Megahaskhal.Internal (Brain)

-- | Seed a first word for the reply words
seed :: Context -> I.Dictionary -> [String] -> State StdGen Int
seed ctx dict keywords
    | childLength == 0 = return 0
    | Prelude.length keywords > 0 = do
        let lookupSymbol = flip S.elemIndexL dict
            valid = catMaybes $ Prelude.map lookupSymbol keywords
        if Prelude.length valid < 1
            then seed ctx dict []
            else do
                ind <- state $ randomR (0, Prelude.length valid - 1)
                return $! valid !! ind
    | otherwise = do
        childIndex <- state $ randomR (0, childLength-1)
        return $! getSymbol $ V.unsafeIndex children childIndex
    where children = getChildren $ head ctx
          childLength = V.length children

-- | Return a random word from the current context.
babble :: Context                       -- ^ Context
       -> I.Dictionary                  -- ^ Dictionary of all the words
       -> [String]                      -- ^ List of keywords
       -> [String]                      -- ^ List of replies used
       -> Bool                          -- ^ Whether the key has been used
       -> State StdGen (Int, Bool)      -- ^ The symbol and whether the key was used
babble ctx dict keywords replies used
    | childLength == 0 = return (0, used)
    | otherwise = do
        position <- state $ randomR (0, childLength-1)
        count <- state $ randomR (0, getUsage lastContext)
        return $! findWordToUse children dict keywords replies used 0 position count
    where lastContext = lastTree ctx
          children = getChildren lastContext
          childLength = V.length children

-- | Find a word to use out of the sequence of trees. This sequence should be
-- appropriately chopped so that it represents exactly how many tree's
-- should be considered.
findWordToUse :: V.Vector Tree        -- ^ Vector of trees to search
              -> I.Dictionary           -- ^ Dictionary of all words
              -> [String]               -- ^ List of keywords
              -> [String]               -- ^ List of reply words used
              -> Bool                   -- ^ Whether the key has been used
              -> Int                    -- ^ Current symbol chosen
              -> Int                    -- ^ Current position
              -> Int                    -- ^ Remaining times to search
              -> (Int, Bool)            -- ^ The symbol and whether the key was used
findWordToUse ctx dict keys replies used symb pos count
    | and [elem word keys,
           or [used, not $ I.isAuxWord word],
           not $ elem word replies] = (symbol, True)
    | otherwise =
        let newCount = count - getCount node
        in if newCount < 0
            then (symbol, used)
            else
                let position = if pos+1 >= (V.length ctx) then 0 else pos+1
                in findWordToUse ctx dict keys replies used symbol position newCount
    where node = V.unsafeIndex ctx pos
          symbol = getSymbol node
          word = S.index dict symbol

reply :: I.Brain                        -- ^ A brain to start with
      -> String                         -- ^ Words to respond to
      -> State StdGen String            -- ^ Reply words
reply brain phrase = do
    let keywords = words . (Prelude.map toUpper) $ phrase
        dict = I.getDictionary brain
        order = I.getOrder brain
        initialCtx = newContext (I.getForward brain) order
    (fWords, usedKey) <- forwardWords initialCtx dict order keywords [] False
    let backCtx = newContext (I.getBackward brain) order
        minCtx = min (Prelude.length fWords) order
        lookupSymbol = flip S.elemIndexL dict
        wordsToUse = Prelude.map lookupSymbol $ Prelude.take minCtx fWords
        newBackCtx = createBackContext backCtx order (reverse wordsToUse)
    (bWords, _) <- backwardWords newBackCtx dict order keywords fWords usedKey
    let phrase = foldr1 (++) $ bWords ++ fWords
        lowered = Prelude.map toLower phrase
        titled = toTitle (head lowered) : tail lowered
    return $ titled

backwardWords :: Context              -- ^ Context to begin with
              -> I.Dictionary           -- ^ Dictionary to use
              -> Int                    -- ^ Order
              -> [String]               -- ^ Keywords entered
              -> [String]               -- ^ Words in the reply so far
              -> Bool                   -- ^ Used the key or not
              -> State StdGen ([String], Bool)
backwardWords ctx dict order keywords replies usedKey = do
    (symbol, newUsedKey) <- babble ctx dict keywords replies usedKey
    if symbol `elem` [0, 1]
        then return ([], newUsedKey)
        else do
            let word = S.index dict symbol
                replyWords = word:replies
                newCtx = updateContext ctx order symbol
            (rest, returnKey) <- backwardWords newCtx dict order keywords replyWords newUsedKey
            return $! (rest ++ [word], returnKey)

forwardWords :: Context               -- ^ Context to begin with
             -> I.Dictionary            -- ^ Dictionary to use
             -> Int                     -- ^ Order
             -> [String]                -- ^ Keywords entered
             -> [String]                -- ^ Words in the reply so far
             -> Bool                    -- ^ Used the key or not
             -> State StdGen ([String], Bool)
forwardWords ctx dict order keywords replies usedKey
    | Prelude.null replies = do
        symbol <- seed ctx dict keywords
        if symbol `elem` [0, 1]
            then return ([], usedKey)
            else do
                let startWord = S.index dict symbol
                    newCtx = updateContext ctx order symbol
                (rest, returnKey) <- forwardWords newCtx dict order keywords [startWord] usedKey
                return $! (startWord:rest, returnKey)
    | otherwise = do
        (symbol, newUsedKey) <- babble ctx dict keywords replies usedKey
        if symbol `elem` [0, 1]
            then return ([], newUsedKey)
            else do
                let word = S.index dict symbol
                    replyWords = word:replies
                    newCtx = updateContext ctx order symbol
                (rest, returnKey) <- forwardWords newCtx dict order keywords replyWords newUsedKey
                return $! (word:rest, returnKey)
