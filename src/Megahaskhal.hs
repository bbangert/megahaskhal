{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Megahaskhal (
    loadBrainFromFilename,
    reply,
    craftReply,
    customCraft,
    getWords,
    capitalizeSentence,
    Brain
    ) where

import           Control.Applicative       ((<$>))
import           Control.Monad             (replicateM)
import           Control.Monad.State       (MonadState, State, state)
import           Data.Char                 (isAlpha, isAlphaNum, isDigit,
                                            toUpper)
import           Data.List                 (foldl', mapAccumL)
import           Data.Maybe                (mapMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           System.Random             (Random, StdGen, randomR)

import           Megahaskhal.Dictionary    (Dictionary, findWord, lookupIndex)
import           Megahaskhal.Internal      (Brain)
import qualified Megahaskhal.Internal      as I
import           Megahaskhal.Replies       (ScoredReply (ScoredReply),
                                            TopReplies)
import qualified Megahaskhal.Replies       as R
import           Megahaskhal.Serialization (loadBrainFromFilename)
import           Megahaskhal.Tree          (Context, Tree, createBackContext,
                                            findSymbol, getChildren, getCount,
                                            getSymbol, getUsage, lastTree,
                                            newContext, updateContext)
import qualified Megahaskhal.Tree          as MT

-- | Aliases for easy reading
type Keywords = [Int]
type Replies = [Int]
type UsedKey = Bool
type Symbol = Int
type Order = Int

data EContext = EContext { eNum     :: {-# UNPACK #-} !Float
                         , eEntropy :: {-# UNPACK #-} !Float
                         , eContext :: !Context }

-- Rules for tokenization:
-- Four character classes: alpha, digit, apostrophe, and other
-- If the character class changed from the previous to current character, then
-- it is a boundary. The only special case is alpha -> apostrophe -> alpha,
-- which is not considered to be a boundary (it's considered to be alpha).
-- If the last word is alphanumeric then add a last word of ".", otherwise
-- replace the last word with "." unless it already ends with one of "!.?".
getWords :: Text -> [Text]
getWords = I.makeKeywords . fixup . T.groupBy sameClass . T.toUpper
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

-- Capitalize a sentence. First word get's capitalized by default along
-- with any word that begins after a sentence ender (.!?)
capitalizeSentence :: Text -> Text
capitalizeSentence = T.unwords . snd . mapAccumL go True . T.words
  where
    capWord w = maybe w (\(c, r) -> T.cons (toUpper c) r) (T.uncons w)
    go acc a
      | acc && isAlpha (T.head a) = (False, capWord a)
      | a == "i"                  = (False, "I")
      | T.last a `elem` "!.?"     = (True, a)
      | otherwise                 = (acc, a)

-- | Craft a reply for a given sample period and return the one with the most
-- surprise
craftReply :: I.Brain -> [Text] -> State StdGen ScoredReply
craftReply brain phrase = maximum <$> replicateM 200 (reply brain phrase)

-- | Craft a custom reply that meets these requirements
customCraft :: (Int, Int)
            -> Brain -> [Text]
            -> State StdGen ScoredReply
customCraft (minLength, maxLength) brain phrase = do
  let bst = R.empty 30
      al = R.empty 30
  times <- state $ randomR (200, 2500)
  replies <- go times bst al
  if R.curCapacity replies < 1
    then customCraft (minLength, maxLength) brain phrase
    else do
      ind <- rndIndex $ R.curCapacity replies - 1
      let ind' = if ind < 0 then 0 else ind
      let repl = R.allReplies replies !! ind'
      return $ repl { R.sReply = capitalizeSentence $ R.sReply repl }
  where
    go :: Int -> TopReplies -> TopReplies -> State StdGen TopReplies
    -- no more iterations, return best matches if avail, otherwise all
    go 0 bst al
        | R.curCapacity bst > 0 = return bst
        | otherwise             = return al
    -- more iterations to do, add this to a best match if it meets the
    -- criteria, otherwise all responses, and iterate again
    go remaining bst al = do
        rep <- reply brain phrase
        let repLen = T.length $ R.sReply rep
            newRem = remaining - 1
        if repLen >= minLength && repLen < maxLength
            then go newRem (R.addReply rep bst) al
            else go newRem bst (R.addReply rep al)

-- | Reply to a phrase with a given brain
reply :: I.Brain                      -- ^ A brain to start with
      -> [Text]                       -- ^ Words to respond to
      -> State StdGen ScoredReply     -- ^ Reply words along with score
reply brain@(I.Brain fTree bTree _ order dict) phrase = do
    let lookupSymbol = flip lookupIndex dict
        kws          = mapMaybe lookupSymbol phrase
        ctx          = newContext fTree order
    symbol <- seed ctx dict kws
    (fWords, fSymbols, usedKey) <- processWords ctx dict order kws [] False symbol
    let wordsToUse   = reverse $ take order fSymbols
        backCtx      = createBackContext bTree order wordsToUse
    (bWords, bSymbols, _) <- autoBabble backCtx dict order kws fSymbols usedKey
    let lowered      = T.toLower . T.concat $ reverse bWords ++ fWords
        surprise = evaluateReply brain kws $ reverse bSymbols ++ fSymbols
    return $ ScoredReply lowered surprise

rndIndex :: MonadState StdGen m => Int -> m Int
rndIndex n = state $ randomR (0, n - 1)

-- Evaluate the 'surprise' factor of a given choice of reply words
-- The surprise factor is based entirely around whether the chosen reply
-- includes words used in the keywords that were supplied.
-- For every word in the reply, the context is built up and iterated over
-- regardless of if the word is a keyword. When a keyword is hit, a subloop
-- runs for that portion over the entire context at that state determining if
-- any of the tree's contain the symbol and updating the probability, at the
-- end of which the entropy is updated.
evaluateReply :: I.Brain -> Keywords -> Replies -> Float
evaluateReply _ [] _ = 0
evaluateReply (I.Brain fTree bTree _ order _) keys repl
  | num < 8   = entropy
  | num < 16  = entropy / sqrt (num-1)
  | otherwise = entropy / sqrt (num-1) / num
  where
    eval = foldl' (evalulateSymbols order keys)
    -- evaluate the words going forward
    fctx = eval (EContext 0 0 (newContext fTree order)) repl
    -- evaluate the words going backwards
    bctx = eval (fctx { eContext = newContext bTree order }) (reverse repl)
    num = eNum bctx
    entropy = eEntropy bctx


-- entropy and num accumulator that retains updated context as each tree
-- in the context is stepped through
evalulateSymbols :: Order
                 -> Keywords
                 -> EContext -- ^ Accumulator
                 -> Int      -- ^ Current symbol
                 -> EContext -- ^ Accumulated value
evalulateSymbols order keys acc@(EContext accNum accEntropy ctx) symbol
  | symbol `elem` keys = EContext (accNum + 1) newEntropy newctx
  | otherwise          = acc { eContext = newctx }
  where
    -- we always update the context with the symbol on each step, even
    -- if the symbol here isn't a keyword
    newctx = updateContext ctx order symbol
    -- our context evaluator for this symbol
    (count, prob) = foldl' (evaluateContext symbol) (0, 0) ctx
    newEntropy | count > 0 = accEntropy - log (prob/count)
               | otherwise = accEntropy

-- context accumulator that returns the count and probability
evaluateContext :: Symbol -> (Float, Float) -> Tree -> (Float,Float)
evaluateContext _ acc tree
  | MT.null tree = acc
evaluateContext symbol (!count, !prob) tree =
  (count + 1, prob + (nodeCount / nodeUsage))
  where
    node = findSymbol tree symbol
    nodeCount = fromIntegral $ getCount node
    nodeUsage = fromIntegral $ getUsage tree

-- | Seed a first word for the reply words
seed :: Context -> Dictionary -> [Int] -> State StdGen Int
seed ctx _dict []
    | childLength == 0  = return 0
    | otherwise         =
      getSymbol . V.unsafeIndex children <$> rndIndex childLength
    where children      = getChildren $ head ctx
          childLength   = V.length children
seed _ctx _dict keywords = do
    ind <- rndIndex (length keywords)
    return $ keywords !! ind

-- | Return a random word from the current context.
babble :: Context -> Dictionary -> Keywords -> Replies -> UsedKey
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
findWordToUse :: V.Vector Tree -> Dictionary -> Keywords -> Replies -> UsedKey -> Symbol
              -> Int                    -- ^ Current position
              -> Int                    -- ^ Remaining times to search
              -> (Symbol, UsedKey)
findWordToUse ctx dict keys replies used _symb pos count
  | symbol `elem` keys &&
    symbol `notElem` replies &&
    (used || not (I.isAuxWord $ findWord dict symbol)) =
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
autoBabble :: Context -> Dictionary -> Order -> Keywords -> Replies -> UsedKey
           -> State StdGen ([Text], [Int], UsedKey)
autoBabble ctx dict order keywords replies usedKey = do
    (symbol, usedKey') <- babble ctx dict keywords replies usedKey
    processWords ctx dict order keywords replies usedKey' symbol

processWords :: Context -> Dictionary -> Order -> Keywords -> Replies -> UsedKey -> Symbol
             -> State StdGen ([Text], [Int], UsedKey)
processWords _ _ _ _ _ usedKey 0 = return ([], [], usedKey)
processWords _ _ _ _ _ usedKey 1 = return ([], [], usedKey)
processWords ctx dict order keywords replies usedKey symbol = do
  (newSymbol, usedKey') <- babble newCtx dict keywords replyWords usedKey
  (rest, symbols, returnKey) <-
    processWords newCtx dict order keywords replyWords usedKey' newSymbol
  return (word:rest, symbol:symbols, returnKey)
  where
    word = findWord dict symbol
    replyWords = symbol:replies
    newCtx = updateContext ctx order symbol
