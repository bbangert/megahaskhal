module Megahaskhal (
    loadBrainFromFilename,
    seed
    ) where

import Data.Word
import System.Random (getStdRandom, randomR)
import Megahaskhal.Serialization (loadBrainFromFilename)
import qualified Megahaskhal.Internal as I
import qualified Data.Sequence as S

seed :: I.Tree -> IO Word16
seed tree
    | childLength == 0 = return 0
    | otherwise = do
        childIndex <- getStdRandom $ randomR (0, childLength-1)
        return $ I.getSymbol . (flip S.index childIndex) . I.getChildren $ tree
    where childLength = S.length $ I.getChildren tree
