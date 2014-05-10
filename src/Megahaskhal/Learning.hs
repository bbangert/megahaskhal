{-| Learning routines

-}
module Megahaskhal.Learning (
    learnFile
  , learnPhrase
  ) where

import           Control.Applicative    ((<$>))
import           Control.DeepSeq        (force)
import           Data.Maybe             (fromJust)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TI

import           Megahaskhal.Dictionary (lookupIndex)
import           Megahaskhal.Internal   (Brain (Brain), addAllWords, addSymbols)
import           Megahaskhal.Reply      (tokenizeWords)

-- |Learn sentences from a plain-text file
learnFile :: Brain -> String -> IO Brain
learnFile brain fileName = do
  s <- T.lines <$> TI.readFile fileName
  return $! go brain s
  where go b []     = b
        go b (l:ls) =
          if T.null nl || isComment
            then go b ls
            else go nb ls
          where nl        = T.strip l
                isComment = T.head nl == '#'
                nb        = learnPhrase b $ tokenizeWords nl

-- |Learn a set of tokenized words
learnPhrase :: Brain -> [Text] -> Brain
learnPhrase (Brain ft bt c o d) p =
  Brain (segments ft symbols)
        (segments bt $ reverse symbols)
        c o newDict
  where
    newDict = addAllWords d p
    symbols = map (fromJust . flip lookupIndex newDict) p
    segments t [] = t
    segments t syms@(_:sx) = segments newTree sx
      where remainingSymbols = take (o+1) syms
            useSymbols
              | length remainingSymbols <= o = remainingSymbols ++ [1]
              | otherwise = remainingSymbols
            newTree = addSymbols t useSymbols
