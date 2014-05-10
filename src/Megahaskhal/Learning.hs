{-| Learning routines

-}
module Megahaskhal.Learning (
    learnFile
  , learnPhrase
  ) where

import           Control.Applicative    ((<$>))
import           Control.DeepSeq        (force)
import           Data.Foldable          (foldl')
import           Data.List              (tails)
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
  return $! foldl' go brain s
  where go b l = if T.null nl || isComment then b
                  else learnPhrase b $ tokenizeWords nl
          where nl        = T.strip l
                isComment = T.head nl == '#'

-- |Learn a set of tokenized words
learnPhrase :: Brain -> [Text] -> Brain
learnPhrase (Brain ft bt c o d) p =
  Brain (foldl' segments ft $ ngrams symbols)
        (foldl' segments bt $ ngrams $ reverse symbols)
        c o newDict
  where
    newDict = addAllWords d p
    symbols = map (fromJust . flip lookupIndex newDict) p
    ngrams s = map (take (o+1)) . tails $ s
    segments t syms = addSymbols t useSymbols
      where useSymbols
              | length syms <= o = syms ++ [1]
              | otherwise = syms
