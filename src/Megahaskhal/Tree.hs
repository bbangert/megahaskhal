{-| Tree and Context manipulation

-}

module Megahaskhal.Tree (
    -- * Context
    -- $context
      Context
    , newContext
    , createBackContext
    , updateContext
    , lastTree

    -- * Tree
    -- $tree
    , Tree (..)

    -- ** Accessors
    , null
    , getSymbol
    , getUsage
    , getCount
    , getChildren
    , findSymbol

    -- ** Construction
    , mkTree

    -- ** Modifying Trees
    , insertTree
    , findSymbolAdd

    -- ** Folding
    , foldl

    ) where

import           Data.List                 (foldl')
import           Data.Vector               (Vector, (!))
import qualified Data.Vector               as IV
import qualified Data.Vector.Fusion.Stream as VS
import qualified Data.Vector.Generic       as V
import           Data.Word                 (Word16, Word32)
import           Prelude                   hiding (foldl, null)

data Tree = Empty
          | Tree { treeSymbol   :: {-# UNPACK #-} !Word16
                 , treeUsage    :: {-# UNPACK #-} !Word32
                 , treeCount    :: {-# UNPACK #-} !Word16
                 , treeChildren :: {-# UNPACK #-} !(Vector Tree)
                 } deriving (Eq, Show)

type Context = Vector Tree

foldl :: (a -> Tree -> a) -> a -> Tree -> a
foldl _ acc Empty = acc
foldl f acc x = IV.foldl (foldl f) acc' $ treeChildren x
    where acc' = f acc x

mkTree :: Word16 -> Word32 -> Word16 -> Vector Tree -> Tree
mkTree = Tree

getSymbol, getUsage, getCount :: Tree -> Int
getSymbol = fromIntegral . treeSymbol
getUsage = fromIntegral . treeUsage
getCount = fromIntegral . treeCount

getChildren :: Tree -> Vector Tree
getChildren = treeChildren

-- | Create a new context initialized with a tree for the given order
-- size. Empty tree's will fill in order slots.
newContext :: Tree -> Int -> Context
newContext t n = IV.fromList $ t : replicate (n-1) Empty

-- | Updates the context by creating a new context where every position in the
-- context is whether the symbol was found in that tree. This operation always
-- preserves the context at the very front.
updateContext :: Context              -- ^ Front portion of context to update
              -> Int                  -- ^ Order
              -> Int                  -- ^ Symbol to locate in the tree
              -> Context              -- ^ The new context
updateContext ctx _ symbol = V.unstream . VS.cons (V.head ctx) . VS.init $ mappedStream
  where
    ctxStream = V.stream ctx
    mappedStream = VS.map symbolFind ctxStream
    symbolFind = flip findSymbol $ symbol

-- | Create a context suitable for navigating backwards based on the symbols
-- used in the current reply using a starting tree.
createBackContext :: Tree -> Int -> [Int] -> Context
createBackContext t order = foldl' (\ctx' s -> updateContext ctx' order s) ctx
    where ctx = newContext t order

-- | Return the last non-Empty tree in a Context sequence
lastTree :: Context -> Tree
lastTree = V.head . V.filter (not . null) . V.reverse

-- | Indicate if a given tree is empty or not
null :: Tree -> Bool
null Empty = True
null _ = False

-- | Insert a tree into the children of an existing tree at the index provided
insertTree :: Tree -> Int -> Tree -> Tree
insertTree newTree index existingTree
  | childLength == 0     = existingTree { treeChildren = V.fromList [newTree] }
  | index >= childLength = existingTree { treeChildren = V.snoc children newTree }
  | otherwise            = existingTree { treeChildren = front V.++ middle V.++ rest }
  where children = treeChildren existingTree
        childLength = V.length children
        (front, rest) = V.splitAt index children
        middle = V.fromList [newTree]

-- | Given a tree and a symbol to locate, perform a binary search on the
-- children of the tree to retrieve a match if possible. Returns an Empty
-- tree if the symbol can't be found.
findSymbol :: Tree -> Int -> Tree
findSymbol Empty _ = Empty
findSymbol t symbol =
  case binsearch children symbol 0 (V.length children - 1) of
    Left _ -> Empty
    Right x -> children ! x
  where children = treeChildren t

-- | Find a symbol in the tree's children if it exists and return it, otherwise
-- create it in the appropriate child node and return it.
findSymbolAdd :: Tree -> Int -> Tree
findSymbolAdd t symbol =
  case binsearch children symbol 0 (childLength - 1) of
    Left z -> newTree z
    Right x -> children ! x
  where children = treeChildren t
        childLength = V.length children
        newSymbolTree = Tree (fromIntegral symbol) 0 0 V.empty
        newTree i = insertTree newSymbolTree i t

binsearch :: IV.Vector Tree -> Int -> Int -> Int -> Either Int Int -- list, value, low, high, return int
binsearch xs value low high
   | high < low       = Left mid
   | pivot > value  = binsearch xs value low (mid-1)
   | pivot < value  = binsearch xs value (mid+1) high
   | otherwise        = Right mid
   where mid = low + ((high - low) `div` 2)
         pivot = getSymbol $ xs ! mid
