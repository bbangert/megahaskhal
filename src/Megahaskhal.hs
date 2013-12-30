module Megahaskhal (
    loadBrainFromFilename,
    reply,
    seed,
    babble
    ) where

import Control.Monad.State (State, state, runState)
import Data.Char (toTitle, toLower, toUpper)
import Data.List (concat)
import Data.Map.Strict as M
import Data.Sequence ( (|>), ViewR( (:>) ), ViewL( (:<) ))
import System.Random (getStdRandom, randomR, StdGen, RandomGen, Random)
import qualified Data.Sequence as S

import Megahaskhal.Serialization (loadBrainFromFilename)
import qualified Megahaskhal.Internal as I


-- | Return the last non-Empty tree in a Context sequence
lastTree :: I.Context -> I.Tree
lastTree ctx =
    let (_, remaining) = S.spanr (I.null) ctx
        _ :> lastContext = S.viewr remaining
    in lastContext


-- | Seed a first word for the reply words
seed :: I.Context -> I.Dictionary -> [String] -> State StdGen Int
seed ctx dict keywords
    | childLength == 0 = return 0
    | Prelude.length keywords > 0 = do
        let lookupSymbol = flip S.elemIndexL dict
            valid = validSymbols $ Prelude.map lookupSymbol keywords
        if Prelude.length valid < 1
            then seed ctx dict []
            else do
                ind <- state $ randomR (0, Prelude.length valid - 1)
                return $ valid !! ind
    | otherwise = do
        childIndex <- state $ randomR (0, childLength-1)
        return $ I.getSymbol . flip S.index childIndex $ children
    where children = I.getChildren $ S.index ctx 0
          childLength = S.length children


validSymbols :: [Maybe Int] -> [Int]
validSymbols [] = []
validSymbols (Nothing:xs) = validSymbols xs
validSymbols ((Just x):xs) = x:validSymbols xs


-- | Return a random word from the current context.
babble :: I.Context                     -- ^ Context
       -> I.Dictionary                  -- ^ Dictionary of all the words
       -> [String]                      -- ^ List of keywords
       -> [String]                      -- ^ List of replies used
       -> Bool                          -- ^ Whether the key has been used
       -> State StdGen (Int, Bool)      -- ^ The symbol and whether the key was used
babble ctx dict keywords replies used
    | childLength == 0 = return (0, used)
    | otherwise = do
        position <- state $ randomR (0, childLength-1)
        count <- state $ randomR (0, I.getUsage lastContext)
        let newTree = searchTree children position (count+1)
        return $ findWordToUse newTree dict keywords replies used 0
    where lastContext = lastTree ctx
          children = I.getChildren lastContext
          childLength = S.length children


-- | Construct a new tree by splitting a sequence tree and constructing
-- a new one no more than the length desired by starting at the split
-- position and continuing as needed.
searchTree :: S.Seq I.Tree
           -> Int                   -- ^ Position to splice
           -> Int                   -- ^ Length of desired sequence
           -> S.Seq I.Tree
searchTree tree position len
    | rLen <= 0 = S.take len end
    | otherwise = end S.>< S.take rLen front
    where (front, end) = S.splitAt position tree
          rLen = len - (S.length end)


-- | Find a word to use out of the sequence of trees. This sequence should be
-- appropriately chopped so that it represents exactly how many tree's
-- should be considered.
findWordToUse :: S.Seq I.Tree           -- ^ Sequence of trees to search
              -> I.Dictionary           -- ^ Dictionary of all words
              -> [String]               -- ^ List of keywords
              -> [String]               -- ^ List of reply words used
              -> Bool                   -- ^ Whether the key has been used
              -> Int                    -- ^ Current symbol chosen
              -> (Int, Bool)            -- ^ The symbol and whether the key was used
findWordToUse ctx _ _ _ used symb
    | S.null ctx = (symb, used)
findWordToUse ctx dict keys replies used symb
    | and [elem word keys,
           or [used, not $ I.isAuxWord word],
           not $ elem word replies] = (symbol, True)
    | otherwise =
        let count = I.getCount node
            remainingCtx = S.drop count ctx
        in findWordToUse remainingCtx dict keys replies used symbol
    where node :< _ = S.viewl ctx
          symbol = I.getSymbol node
          word = S.index dict symbol


reply :: I.Brain                        -- ^ A brain to start with
      -> String                         -- ^ Words to respond to
      -> State StdGen String            -- ^ Reply words
reply brain phrase = do
    let keywords = words . (Prelude.map toUpper) $ phrase
        dict = I.getDictionary brain
        order = I.getOrder brain
        empties = take order $ repeat I.Empty
        initialCtx = S.fromList $ I.getForward brain : empties
    (fWords, usedKey) <- forwardWords initialCtx dict order keywords [] False
    let backCtx = S.fromList $ I.getBackward brain : empties
        minCtx = min (Prelude.length fWords) order
        lookupSymbol = flip S.elemIndexL dict
        wordsToUse = Prelude.map lookupSymbol $ Prelude.take minCtx fWords
        newBackCtx = createBackContext backCtx (reverse wordsToUse)
    (bWords, _) <- backwardWords newBackCtx dict order keywords fWords usedKey
    let phrase = foldr1 (++) $ bWords ++ fWords
        lowered = Prelude.map toLower phrase
        titled = toTitle (head lowered) : tail lowered
    return $ titled


-- | Create a context suitable for navigating backwards based on the symbols
-- used in the current reply.
createBackContext :: I.Context -> [Maybe Int] -> I.Context
createBackContext ctx [] = ctx
createBackContext ctx (Nothing:xs) = createBackContext ctx xs
createBackContext ctx ((Just w):xs) = createBackContext (updateContext ctx w) xs


backwardWords :: I.Context              -- ^ Context to begin with
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
                newCtx = S.take (order+1) $ updateContext ctx symbol
            (rest, returnKey) <- backwardWords newCtx dict order keywords replyWords newUsedKey
            return (rest ++ [word], returnKey)


forwardWords :: I.Context               -- ^ Context to begin with
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
                    newCtx = S.take (order+1) $ updateContext (ctx |> I.Empty) symbol
                (rest, returnKey) <- forwardWords newCtx dict order keywords [startWord] usedKey
                return (startWord:rest, returnKey)
    | otherwise = do
        (symbol, newUsedKey) <- babble ctx dict keywords replies usedKey
        if symbol `elem` [0, 1]
            then return ([], newUsedKey)
            else do
                let word = S.index dict symbol
                    replyWords = word:replies
                    newCtx = S.take (order+1) $ updateContext (ctx |> I.Empty) symbol
                (rest, returnKey) <- forwardWords newCtx dict order keywords replyWords newUsedKey
                return (word:rest, returnKey)


updateContext :: I.Context              -- ^ Front portion of context to update
              -> Int                    -- ^ Symbol to locate in the tree
              -> I.Context              -- ^ The updated context
updateContext ctx _
    | S.length ctx <= 1     = ctx
updateContext ctx symbol
    | I.null prior          = updateContext fnt symbol |> cur
    | otherwise             = updateContext fnt symbol |> findSymbol prior symbol
    where fnt :> cur   = S.viewr ctx
          _   :> prior = S.viewr fnt


findSymbol :: I.Tree -> Int -> I.Tree
findSymbol t symbol
    | S.length foundNodes < 1 = I.Empty
    | otherwise = let fnt :< _ = S.viewl foundNodes in fnt
    where children = I.getChildren t
          foundNodes = S.filter ((==symbol) . I.getSymbol) children
