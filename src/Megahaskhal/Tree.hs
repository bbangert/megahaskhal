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
    , addSymbol
    , addSymbols

    -- ** Folding
    , foldl

    ) where

import           Data.List                   (foldl')
import           Data.Vector                 (Vector, (!))
import qualified Data.Vector                 as IV
import qualified Data.Vector.Fusion.Stream   as VS
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as VM
import           Data.Word                   (Word16, Word32)
import           Prelude                     hiding (foldl, null)

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
insertTree :: Tree -> Tree -> Tree
insertTree newTree existingTree
  | childLength == 0     = updateTree $ V.fromList [newTree]
  | otherwise            = updateTree $ V.concat [front, middle, rest]
  where children = treeChildren existingTree
        childLength = V.length children
        lowerNode n = (treeSymbol n < treeSymbol newTree)
        (front, rest) = V.partition lowerNode children
        middle = V.fromList [newTree]
        updateTree c = existingTree { treeChildren=c }

-- | Given a tree and a symbol to locate, perform a binary search on the
-- children of the tree to retrieve a match if possible. Returns an Empty
-- tree if the symbol can't be found.
findSymbol :: Tree -> Int -> Tree
findSymbol Empty _ = Empty
findSymbol t symbol =
  case binsearch children symbol 0 (V.length children - 1) of
    Nothing -> Empty
    Just x -> children ! x
  where children = treeChildren t

-- | Find a symbol in the tree's children if it exists and return it, otherwise
-- create it in the appropriate child node and return it.
findSymbolAdd :: Tree -> Int -> (Int, Tree)
findSymbolAdd t symbol =
  case binsearch children symbol 0 (childLength - 1) of
    Nothing ->
        let nt = newTree
            (loc, _) = findSymbolAdd nt symbol
        in (loc, nt)
    Just x -> (x, t)
  where children = treeChildren t
        childLength = V.length children
        newSymbolTree = Tree (fromIntegral symbol) 0 0 V.empty
        newTree = insertTree newSymbolTree t

-- | Replaces a child node in a tree at the given index with a new tree node
-- and returns the updated tree
replaceTree :: Tree -> Int -> Tree -> Tree
replaceTree oldTree index node =
  oldTree { treeChildren = newChildren }
  where newChildren = V.modify (\v -> VM.write v index node) $ treeChildren oldTree

-- | Add a symbol's usage to a tree and return the updated tree.
addSymbol :: Tree -> Int -> Tree
addSymbol t symbol =
    (replaceTree tree index newNode) { treeUsage = curTreeUsage+1}
    where (index, tree) = findSymbolAdd t symbol
          children      = treeChildren tree
          node          = children ! index
          curNodeCount  = treeCount node
          curTreeUsage  = treeUsage tree
          newNode       = node { treeCount = curNodeCount+1 }

-- | Add a list of symbols to the tree navigating down and return it
addSymbols :: Tree -> [Int] -> Tree
addSymbols t [] = t
addSymbols t (w:ws) = replaceTree t i newSymbolTree
  where
    -- A new tree with the symbol added and counts incremented
    nt = addSymbol t w
    -- Grab the index of the symbol added/updated
    (i, _) = findSymbolAdd nt w
    children = treeChildren nt
    -- Get the tree node itself
    node = children ! i
    -- Update the tree node with the remaining symbols
    newSymbolTree = addSymbols node ws

binsearch :: IV.Vector Tree -> Int -> Int -> Int -> Maybe Int -- list, value, low, high, return int
binsearch xs value low high
   | high < low       = Nothing
   | pivot > value  = binsearch xs value low (mid-1)
   | pivot < value  = binsearch xs value (mid+1) high
   | otherwise        = Just mid
   where mid = low + ((high - low) `div` 2)
         pivot = getSymbol $ xs ! mid
