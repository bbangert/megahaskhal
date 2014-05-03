module Megahaskhal (
    Brain
  , getWords
  , loadBrainFromFilename
  ) where

import           Megahaskhal.Internal      (Brain)
import           Megahaskhal.Reply         (getWords)
import           Megahaskhal.Serialization (loadBrainFromFilename)
