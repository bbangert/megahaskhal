module Megahaskhal.Tree (
    Context,
    Tree (Tree),
    getSymbol,
    getUsage,
    getCount,
    getChildren,
    null,
    lastTree,
    findSymbol,
    createBackContext,
    newContext,
    updateContext
    ) where

import Prelude hiding (null)
import Data.List (foldl')
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

data Tree = Empty | Tree {
    getSymbol     :: {-# UNPACK #-} !Int
    , getUsage    :: {-# UNPACK #-} !Int
    , getCount    :: {-# UNPACK #-} !Int
    , getChildren :: {-# UNPACK #-} !(Vector Tree)
    } deriving (Eq, Show)

type Context = [Tree]

-- | Create a new context initialized with a tree for the given order
-- size. Empty tree's will fill in order slots.
newContext :: Tree -> Int -> Context
newContext t n = t : replicate n Empty

-- | Updates the context by creating a new context that has the symbol located
-- in the context where possible that will be 'order+!' left.
updateContext :: Context              -- ^ Front portion of context to update
              -> Int                  -- ^ Order
              -> Int                  -- ^ Symbol to locate in the tree
              -> Context              -- ^ The new context
updateContext ctx order symbol =
  head ctx : updateContext' (take (order-1) ctx) symbol

updateContext' :: Context -> Int -> Context
updateContext' ctx symbol = map (`findSymbol` symbol) ctx

-- | Create a context suitable for navigating backwards based on the symbols
-- used in the current reply using a starting tree.
createBackContext :: Tree -> Int -> [Int] -> Context
createBackContext t order = foldl' (\_ s -> updateContext ctx order s) []
    where ctx = newContext t order

-- | Indicate if a given tree is empty or not
null :: Tree -> Bool
null Empty = True
null _ = False

-- | Return the last non-Empty tree in a Context sequence
lastTree :: Context -> Tree
lastTree = last . filter (not . null)

-- | Given a tree and a symbol to locate, perform a binary search on the
-- children of the tree to retrieve a match if possible. Returns an Empty
-- tree if the symbol can't be found.
findSymbol :: Tree -> Int -> Tree
findSymbol Empty _ = Empty
findSymbol t symbol =
    let children = getChildren t
        node = binsearch children symbol 0 (V.length children - 1)
    in case node of
        Nothing -> Empty
        Just x -> children ! x

binsearch :: V.Vector Tree -> Int -> Int -> Int -> Maybe Int -- list, value, low, high, return int
binsearch xs value low high
   | high < low       = Nothing
   | pivot > value  = binsearch xs value low (mid-1)
   | pivot < value  = binsearch xs value (mid+1) high
   | otherwise        = Just mid
   where mid = low + ((high - low) `div` 2)
         pivot = getSymbol $! xs ! mid
