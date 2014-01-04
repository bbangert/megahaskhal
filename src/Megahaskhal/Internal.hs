{-# LANGUAGE OverloadedStrings #-}
module Megahaskhal.Internal
       ( auxWords
       , isAuxWord
       , makeKeywords
       , newBrainOrder
       , Brain(..)
       ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Megahaskhal.Tree (Tree)
import Megahaskhal.Dictionary (Dictionary)

auxWords :: S.Set Text
auxWords =
  S.fromList [ "DISLIKE", "HE", "HER", "HERS", "HIM", "HIS", "I", "I'D"
             , "I'LL", "I'M", "I'VE", "LIKE", "ME", "MY", "MYSELF", "ONE"
             , "SHE", "THREE", "TWO", "YOU", "YOU'D", "YOU'LL", "YOU'RE"
             , "YOU'VE", "YOUR", "YOURSELF" ]

banWords :: S.Set Text
banWords =
  S.fromList ["A", "ABILITY", "ABLE", "ABOUT", "ABSOLUTE", "ABSOLUTELY", "ACROSS", "ACTUAL", "ACTUALLY", "AFTER",
              "AGAIN", "AGAINST", "AGO", "AGREE", "ALL", "ALMOST", "ALONG", "ALREADY", "ALTHOUGH", "ALWAYS",
              "AN", "AND", "ANOTHER", "ANY", "ANYHOW", "ANYTHING", "ANYWAY", "ARE", "AREN'T", "AROUND", "AS",
              "AWAY", "BACK", "BAD", "BE", "BEEN", "BEFORE", "BEHIND", "BEING", "BELIEVE", "BELONG", "BEST",
              "BETWEEN", "BIG", "BIGGER", "BIGGEST", "BIT", "BOTH", "BUDDY", "BUT", "BY", "CALL", "CALLED",
              "CAME", "CAN", "CAN'T", "CANNOT", "CARE", "CARING", "CASE", "CATCH", "CAUGHT", "CERTAIN",
              "CHANGE", "CLOSE", "CLOSER", "COME", "COMING", "COMMON", "CONSTANT", "CONSTANTLY", "COULD",
              "DAY", "DAYS", "DERIVED", "DESCRIBE", "DESCRIBES", "DETERMINE", "DETERMINES", "DID", "DIDN'T",
              "DOES", "DOESN'T", "DOING", "DON'T", "DONE", "DOUBT", "DOWN", "EACH", "EARLIER", "EARLY", "ELSE",
              "ESPECIALLY", "EVEN", "EVER", "EVERY", "EVERYBODY", "EVERYONE", "EVERYTHING", "FACT", "FAIR",
              "FAR", "FELLOW", "FEW", "FIND", "FINE", "FOR", "FORM", "FOUND", "FROM", "FULL", "FURTHER", "GAVE",
              "GETTING", "GIVE", "GIVEN", "GIVING", "GO", "GOING", "GONE", "GOOD", "GOT", "GOTTEN", "GREAT",
              "HAS", "HASN'T", "HAVE", "HAVEN'T", "HAVING", "HELD", "HERE", "HIGH", "HOLD", "HOLDING", "HOW",
              "IN", "INDEED", "INSIDE", "INSTEAD", "INTO", "IS", "ISN'T", "IT", "IT'S", "ITS", "JUST", "KEEP",
              "KNEW", "KNOW", "KNOWN", "LARGE", "LARGER", "LARGETS", "LAST", "LATE", "LATER", "LEAST", "LESS",
              "LET'S", "LEVEL", "LIKES", "LITTLE", "LONG", "LONGER", "LOOK", "LOOKED", "LOOKING", "LOOKS", "LOW",
              "MAKE", "MAKING", "MANY", "MATE", "MAY", "MAYBE", "MEAN", "MEET", "MENTION", "MERE", "MIGHT",
              "MORE", "MORNING", "MOST", "MOVE", "MUCH", "MUST", "NEAR", "NEARER", "NEVER", "NEXT", "NICE",
              "NONE", "NOON", "NOONE", "NOT", "NOTE", "NOTHING", "NOW", "OBVIOUS", "OF", "OFF", "ON", "ONCE",
              "ONTO", "OPINION", "OR", "OTHER", "OUR", "OUT", "OVER", "OWN", "PART", "PARTICULAR",
              "PERHAPS", "PERSON", "PIECE", "PLACE", "PLEASANT", "PLEASE", "POPULAR", "PREFER", "PRETTY", "PUT",
              "REAL", "REALLY", "RECEIVE", "RECEIVED", "RECENT", "RECENTLY", "RELATED", "RESULT", "RESULTING",
              "SAID", "SAME", "SAW", "SAY", "SAYING", "SEE", "SEEM", "SEEMED", "SEEMS", "SEEN", "SELDOM",
              "SET", "SEVERAL", "SHALL", "SHORT", "SHORTER", "SHOULD", "SHOW", "SHOWS", "SIMPLE", "SIMPLY",
              "SO", "SOME", "SOMEONE", "SOMETHING", "SOMETIME", "SOMETIMES", "SOMEWHERE", "SORT", "SORTS",
              "SPENT", "STILL", "STUFF", "SUCH", "SUGGEST", "SUGGESTION", "SUPPOSE", "SURE", "SURELY",
              "SURROUNDS", "TAKE", "TAKEN", "TAKING", "TELL", "THAN", "THANK", "THANKS", "THAT", "THAT'S",
              "THE", "THEIR", "THEM", "THEN", "THERE", "THEREFORE", "THESE", "THEY", "THING", "THINGS", "THIS",
              "THOUGH", "THOUGHTS", "THOUROUGHLY", "THROUGH", "TINY", "TO", "TODAY", "TOGETHER", "TOLD",
              "TOO", "TOTAL", "TOTALLY", "TOUCH", "TRY", "TWICE", "UNDER", "UNDERSTAND", "UNDERSTOOD", "UNTIL",
              "US", "USED", "USING", "USUALLY", "VARIOUS", "VERY", "WANT", "WANTED", "WANTS", "WAS", "WATCH",
              "WAYS", "WE", "WE'RE", "WELL", "WENT", "WERE", "WHAT", "WHAT'S", "WHATEVER", "WHATS", "WHEN",
              "WHERE'S", "WHICH", "WHILE", "WHILST", "WHO", "WHO'S", "WHOM", "WILL", "WISH", "WITH", "WITHIN",
              "WONDERFUL", "WORSE", "WORST", "WOULD", "WRONG", "YESTERDAY", "YET"]

swapWords :: M.Map Text Text
swapWords =
  M.fromList [("YOU'RE", "I'M"), ("YOU'D", "I'D"), ("HATE", "LOVE"), ("YOUR", "MY"), ("I'LL", "YOU'LL"), ("NO", "YES"),
              ("WHY", "BECAUSE"), ("YOU", "ME"), ("LOVE", "HATE"), ("I", "YOU"), ("MINE", "YOURS"), ("YOURSELF", "MYSELF"),
              ("DISLIKE", "LIKE"), ("I'M", "YOU'RE"), ("ME", "YOU"), ("MYSELF", "YOURSELF"), ("LIKE", "DISLIKE"),
              ("I'D", "YOU'D"), ("YOU'VE", "I'VE"), ("YES", "NO"), ("MY", "YOUR")]

errorWord :: Text
errorWord = "<ERROR>"

data Brain = Brain {
    getForward :: Tree
    , getBackward :: Tree
    , getCookie :: Text
    , getOrder :: Int
    , getDictionary :: Dictionary
    } deriving (Show)

isAuxWord :: Text -> Bool
isAuxWord = (`S.member` auxWords)

isBanWord :: Text -> Bool
isBanWord = (`S.member` banWords)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = go S.empty
    where
      go _ [] = []
      go s (x:xs)
        | S.member x s = go s xs
        | otherwise    = x : go (S.insert x s) xs

swapIfPossible :: Text -> Text
swapIfPossible w = M.findWithDefault w w swapWords

-- Indicates a word that is an error or doesn't begin with alphanumeric
undesiredWord :: Text -> Bool
undesiredWord w = (not . isAlphaNum $ T.head w) || w == errorWord

isBanAuxword :: Text -> Bool
isBanAuxword w = isBanWord w || isAuxWord w

-- Filter out banwords, switch in swap words, remove error words, allow
-- aux words if keywords were found in the firstBatch which doesn't
-- allow for aux/banwords
makeKeywords :: [Text] -> [Text]
makeKeywords lst = removeDuplicates $ firstBatch ++ secondBatch
  where
    desiredWords = filter (not . undesiredWord) lst
    swapped = map swapIfPossible desiredWords
    firstBatch = filter (not . isBanAuxword) swapped
    secondBatch = filter isAuxWord swapped

newBrainOrder :: Brain -> Int -> Brain
newBrainOrder ob ord = ob { getOrder = ord }
