module Megahaskhal (
    loadBrainFromFilename
    ) where

import Data.Word
import System.Random (getStdRandom, randomR)
import Megahaskhal.Serialization (loadBrainFromFilename)
import qualified Megahaskhal.Internal as I

seed :: Tree -> IO Word16
seed t =
    | childLength == 0 = return 0
    | otherwise = do
        let childIndex = getStdRandom (randomR (1, childLength))
        return $ I.getSymbol $ I.getChildren t !! childIndex
    where childLength = length $ I.getChildren t
