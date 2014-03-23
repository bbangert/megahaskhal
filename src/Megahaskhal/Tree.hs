module Megahaskhal.Tree (
    Context,
    Tree (..),
    mkTree,
    getSymbol,
    getUsage,
    getCount,
    getChildren,
    null,
    lastTree,
    findSymbol,
    createBackContext,
    newContext,
    updateContext,
    foldl
    ) where

import           Control.Monad       (forM_)
import           Control.Monad.ST    (ST, runST)
import           Data.List           (foldl')
import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as IV
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Mutable as VM
import           Data.Word           (Word16, Word32)
import           Prelude             hiding (foldl, null)

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
newContext t n = IV.fromList $ t : replicate n Empty

-- | Updates the context by creating a new context that has the symbol located
-- in the context where possible that will be 'order+1' left.
updateContext :: Context              -- ^ Front portion of context to update
              -> Int                  -- ^ Order
              -> Int                  -- ^ Symbol to locate in the tree
              -> Context              -- ^ The new context
updateContext ctx _ symbol = runST $ updateContext' symbol ctx

updateContext' :: Int -> Context -> ST s Context
updateContext' symbol ctx = do
  vec <- V.thaw ctx
  let vecLen = VM.length vec - 1
  forM_ (reverse [1..vecLen]) $ \index -> do
    prior <- VM.unsafeRead vec (index-1)
    VM.unsafeWrite vec index $ findSymbol prior symbol
  V.unsafeFreeze vec

-- | Create a context suitable for navigating backwards based on the symbols
-- used in the current reply using a starting tree.
createBackContext :: Tree -> Int -> [Int] -> Context
createBackContext t order = foldl' (\ctx' s -> updateContext ctx' order s) ctx
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
  case binsearch children symbol 0 (V.length children - 1) of
    Left _ -> Empty
    Right x -> children ! x
  where children = treeChildren t
  where children = treeChildren t

binsearch :: IV.Vector Tree -> Int -> Int -> Int -> Either Int Int -- list, value, low, high, return int
binsearch xs value low high
   | high < low       = Left mid
   | pivot > value  = binsearch xs value low (mid-1)
   | pivot < value  = binsearch xs value (mid+1) high
   | otherwise        = Right mid
   where mid = low + ((high - low) `div` 2)
         pivot = getSymbol $ xs ! mid
