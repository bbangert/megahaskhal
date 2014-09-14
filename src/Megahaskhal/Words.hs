{- | Word manipulation functions

-}

{-# LANGUAGE OverloadedStrings #-}

module Megahaskhal.Words (
    getWords
  , tokenizeWords
  , capitalizeSentence

  ) where

import           Data.Char            (isAlpha, isAlphaNum, isDigit, toUpper)
import           Data.List            (mapAccumL)
import           Data.Text            (Text)
import qualified Data.Text            as T

import qualified Megahaskhal.Internal as I

{-| Transform a single @Text@ phrase into its component parts suitable to
    be fed into a reply generating function.

Rules for tokenization:
Four character classes: alpha, digit, apostrophe, and other

If the character class changed from the previous to current character,
then it is a boundary. The only special case is alpha -> apostrophe ->
alpha, which is not considered to be a boundary (it's considered to be
alpha).

If the last word is alphanumeric then add a last word of ".", otherwise
replace the last word with "." unless it already ends with one of
"!.?".

-}
getWords :: Text -> [Text]
getWords = I.makeKeywords . tokenizeWords

tokenizeWords :: Text -> [Text]
tokenizeWords = fixup . T.groupBy sameClass . T.toUpper
  where
    firstAlpha = isAlpha . T.head
    -- find boundaries
    sameClass a b = isAlpha a == isAlpha b && isDigit a == isDigit b
    -- fix apostrophes
    fixup (a:b:c:rest)
      | firstAlpha a && b == "\'" && firstAlpha c =
        fixup (T.concat [a, b, c] : rest)
    -- fix the last word
    fixup (a:[])
      | isAlphaNum (T.head a) = [a, "."]
      | T.last a `elem` "!.?" = [a]
      | otherwise             = ["."]
    -- simple recursive case
    fixup (a:rest) = a : fixup rest
    -- handle empty input
    fixup [] = []

{-| Capitalize a sentence as best as possible given a return @Text@ from a
    reply generating function.

-}
capitalizeSentence :: Text -> Text
capitalizeSentence = T.unwords . snd . mapAccumL go True . T.words
  where
    capWord w = maybe w (\(c, r) -> T.cons (toUpper c) r) (T.uncons w)
    go acc a
      | acc && isAlpha (T.head a) = (False, capWord a)
      | a == "i"                  = (False, "I")
      | T.last a `elem` "!.?"     = (True, a)
      | otherwise                 = (acc, a)
