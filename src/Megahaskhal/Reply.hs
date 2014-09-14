{- |Core reply generation facilities

These functions provide various ways to generate, filter, and restrict
the final set of replies created in response to a phrase for a supplied
'Megahaskhal.Internal.Brain'.

Example usage to generate a response provided a brain:

>  import qualified Data.Text as T
>  import Megahaskhal       (Brain)
>  import Megahaskhal.Reply (generateReply, getWords)
>
>  printReply :: Brain -> IO T.Text
>  printReply brain = do
>    phrase <- getLine
>    generateReply brain (getWords phrase)

-}

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Megahaskhal.Reply (
  generateReply

  -- * Replies
  -- $replies
  , ScoredReply (..)
  , TopReplies (..)
  , empty
  , addReply
  , reply

  ) where

import           Control.Applicative    ((<$>))
import           Control.Monad.State    (MonadState, State, runState, state)
import           Data.List              (foldl')
import qualified Data.List.Ordered      as O
import           Data.Maybe             (mapMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           System.Random          (Random, StdGen, getStdGen, randomR,
                                         setStdGen)

import           Megahaskhal.Dictionary (Dictionary, findWord, lookupIndex)
import           Megahaskhal.Internal   (Brain (..))
import qualified Megahaskhal.Internal   as I
import           Megahaskhal.Tree       (Context, Tree, createBackContext,
                                         findSymbol, getChildren, getCount,
                                         getSymbol, getUsage, lastTree,
                                         newContext, updateContext)
import qualified Megahaskhal.Tree       as MT
import           Megahaskhal.Words      (capitalizeSentence)

-- | Aliases for easy reading
type Keywords = [Int]
type Replies = [Int]
type UsedKey = Bool
type Symbol = Int
type Order = Int

data EContext = EContext { eNum     :: {-# UNPACK #-} !Float
                         , eEntropy :: {-# UNPACK #-} !Float
                         , eContext :: !Context }

-- | Craft a custom reply that meets these requirements
customCraft :: (Int, Int)
            -> Brain -> [Text]
            -> State StdGen ScoredReply
customCraft (minLength, maxLength) brain phrase = do
  let bst = empty 30
      al = empty 30
  times <- state $ randomR (200, 2500)
  replies <- go times bst al
  if curCapacity replies < 1
    then customCraft (minLength, maxLength) brain phrase
    else do
      ind <- rndIndex $ curCapacity replies - 1
      let ind' = max 0 ind
          repl = allReplies replies !! ind'
      return $ repl { sReply = capitalizeSentence $ sReply repl }
  where
    go :: Int -> TopReplies -> TopReplies -> State StdGen TopReplies
    -- no more iterations, return best matches if avail, otherwise all
    go 0 bst al
        | curCapacity bst > 0 = return bst
        | otherwise             = return al
    -- more iterations to do, add this to a best match if it meets the
    -- criteria, otherwise all responses, and iterate again
    go remaining bst al = do
        rep <- reply brain phrase
        let repLen = T.length $ sReply rep
            newRem = remaining - 1
        if repLen >= minLength && repLen < maxLength
            then go newRem (addReply rep bst) al
            else go newRem bst (addReply rep al)

generateReply :: Brain -> [Text] -> IO Text
generateReply brain phrase = do
  gen <- getStdGen
  let (response, newGen) = runState (customCraft (50, 50000) brain phrase) gen
  setStdGen newGen
  return $ sReply response

{- $replies
    Reply creation basic components.

-}

-- | A reply along with its associated score.
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
    | any (replyEquals s) replies = t
    | cC < mC   = TopReplies mC (cC+1) $ O.insertBag s replies
    | s > h     = t { allReplies = O.insertBag s rst }
    | otherwise = t
    where replyEquals x y = sReply x == sReply y

-- | Reply to a phrase with a given brain
reply :: Brain                        -- ^ A brain to start with
      -> [Text]                       -- ^ Words to respond to
      -> State StdGen ScoredReply     -- ^ Reply words along with score
reply brain@(Brain fTree bTree _ order dict) phrase = do
  symbol <- seed ctx dict kws
  (fWords, fSymbols, usedKey) <- processWords ctx dict order kws [] False symbol
  let wordsToUse   = reverse $ take order fSymbols
      backCtx      = createBackContext bTree order wordsToUse
  (bWords, bSymbols, _) <- autoBabble backCtx dict order kws fSymbols usedKey
  let lowered      = T.toLower . T.concat $ reverse bWords ++ fWords
      surprise = evaluateReply brain kws $ reverse bSymbols ++ fSymbols
  return $ ScoredReply lowered surprise
  where
    lookupSymbol = flip lookupIndex dict
    kws          = mapMaybe lookupSymbol phrase
    ctx          = newContext fTree order

rndIndex :: MonadState StdGen m => Int -> m Int
rndIndex n = state $ randomR (0, n - 1)

-- | Seed a first word for the reply words
seed :: Context -> Dictionary -> [Int] -> State StdGen Int
seed ctx _dict []
    | childLength == 0  = return 0
    | otherwise         =
      getSymbol . V.unsafeIndex children <$> rndIndex childLength
    where children      = getChildren $ V.head ctx
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

{-| Evaluate the 'surprise' factor of a given choice of reply words

The surprise factor is based entirely around whether the chosen reply
includes words used in the keywords that were supplied. For every word
in the reply, the context is built up and iterated over regardless of
if the word is a keyword. When a keyword is hit, a subloop runs for
that portion over the entire context at that state determining if any
of the tree's contain the symbol and updating the probability, at the
end of which the entropy is updated.

-}
evaluateReply :: Brain -> Keywords -> Replies -> Float
evaluateReply (Brain fTree bTree _ order _) keys repl
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

{- entropy and num accumulator that retains updated context as each tree
in the context is stepped through -}
evalulateSymbols :: Order
                 -> Keywords
                 -> EContext -- ^ Accumulator
                 -> Int      -- ^ Current symbol
                 -> EContext -- ^ Accumulated value
evalulateSymbols order keys acc@(EContext accNum accEntropy ctx) symbol
  | symbol `elem` keys
    || null keys = EContext (accNum + 1) newEntropy newctx
  | otherwise          = acc { eContext = newctx }
  where
    -- we always update the context with the symbol on each step, even
    -- if the symbol here isn't a keyword
    newctx = updateContext ctx order symbol
    -- our context evaluator for this symbol
    (count, prob) = V.foldl' (evaluateContext symbol) (0, 0) ctx
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
