module Megahaskhal (
    Brain
  , getWords
  , loadBrainFromFilename
  ) where

import           Megahaskhal.Internal      (Brain)
import           Megahaskhal.Words         (getWords)
import           Megahaskhal.Serialization (loadBrainFromFilename)
