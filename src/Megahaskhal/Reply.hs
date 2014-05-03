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
  -- * Pipe Components
  -- $pipes
    replyProducer
  , collectTopReplies
  , generateReply

  -- * Replies
  -- $replies
  , ScoredReply (..)
  , TopReplies (..)
  , empty
  , addReply
  , reply

  -- * Utilities
  -- $utilities
  , getWords
  , capitalizeSentence

  ) where

import           Control.Applicative        ((<$>))
import           Control.Monad.State        (MonadState, State, runState, state)
import           Control.Monad.State.Strict (evalStateT)
import           Data.Char                  (isAlpha, isAlphaNum, isDigit,
                                             toUpper)
import           Data.List                  (foldl', mapAccumL)
import qualified Data.List.Ordered          as O
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Pipes                      (Pipe, Producer, await, lift, yield,
                                             (>->))
import           Pipes.Parse                (Parser, draw)
import qualified Pipes.Prelude              as PL
import           System.Random              (Random, StdGen, getStdGen,
                                             getStdRandom, randomR, setStdGen)


import           Megahaskhal.Dictionary     (Dictionary, findWord, lookupIndex)
import           Megahaskhal.Internal       (Brain (..))
import qualified Megahaskhal.Internal       as I
import           Megahaskhal.Tree           (Context, Tree, createBackContext,
                                             findSymbol, getChildren, getCount,
                                             getSymbol, getUsage, lastTree,
                                             newContext, updateContext)
import qualified Megahaskhal.Tree           as MT

-- | Aliases for easy reading
type Keywords = [Int]
type Replies = [Int]
type UsedKey = Bool
type Symbol = Int
type Order = Int

data EContext = EContext { eNum     :: {-# UNPACK #-} !Float
                         , eEntropy :: {-# UNPACK #-} !Float
                         , eContext :: !Context }


{- $pipes
    Pipe Components utilize the 'Pipes' package for easy composition
    of generated replies to restrict the outcome to a desired set of
    traits.

-}

-- | Produce an endless stream of replies to a given phrase
replyProducer :: Brain -> [Text] -> Producer ScoredReply IO ()
replyProducer brain phrase = do
  gen <- lift getStdGen
  let (response, newGen) = runState (reply brain phrase) gen
  yield response
  lift $ setStdGen newGen
  replyProducer brain phrase

dropOutsideBounds :: Int -> Int -> Pipe ScoredReply ScoredReply IO ()
dropOutsideBounds l u = do
  repl <- await
  let repLen = T.length $ sReply repl
  if l <= repLen && repLen <= u
    then yield repl >> dropOutsideBounds l u
    else dropOutsideBounds l u

-- | Collect top replies
collectTopReplies :: TopReplies -> Parser ScoredReply IO TopReplies
collectTopReplies tr = do
    repl <- draw
    case repl of
        Nothing -> return tr
        Just r  -> collectTopReplies (addReply r tr)

-- | Generate a suitable amount of replies and return the highest scored
generateReply :: Brain -> [Text] -> IO Text
generateReply brain phrase = do
    times <- getStdRandom (randomR (800,1500))
    replies <- evalStateT parser $
        producer >-> dropOutsideBounds 25 5000
                 >-> PL.take times
    let cc = length $ allReplies replies
    index <- getStdRandom (randomR (0, max 0 (cc-1)))
    let repl = allReplies replies !! index
    return $ capitalizeSentence . sReply $ repl
    -- return $ repl { sReply=(capitalizeSentence . sReply) repl }
    where producer = replyProducer brain phrase
          parser   = collectTopReplies $ empty 20

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

{- $utilities
    Utility functions to help transform input for reply generation and
    reply output for human comprehension.

-}

{-| Transform a single @Text@ phrase into its component parts suitable to
    be fed into a reply generating function.

Rules for tokenization:
Four character classes: alpha, digit, apostrophe, and other

If the character class changed from the previous to current character,
then it is a boundary. The only special case is alpha -> apostrophe ->
alpha, which is not considered to be a boundary (it's considered to be
alpha).

If the last word is alphanumeric then add a last word of ".", otherwise
replace the last word with "." unless it already ends with one of
"!.?".

-}
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

{-| Capitalize a sentence as best as possible given a return @Text@ from a
    reply generating function.

-}
capitalizeSentence :: Text -> Text
capitalizeSentence = T.unwords . snd . mapAccumL go True . T.words
  where
    capWord w = maybe w (\(c, r) -> T.cons (toUpper c) r) (T.uncons w)
    go acc a
      | acc && isAlpha (T.head a) = (False, capWord a)
      | a == "i"                  = (False, "I")
      | T.last a `elem` "!.?"     = (True, a)
      | otherwise                 = (acc, a)
