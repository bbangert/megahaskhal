module Main where

import System.Environment (getArgs)
import System.IO  (withFile, IOMode( ReadMode ))
import Control.Monad (replicateM)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import Data.Word

data Tree = Tree {
    getSymbol :: !Word16
    , getUsage :: !Word32
    , getCount :: !Word16
    , getChildren :: [Tree]
    } deriving (Show)

main :: IO ()
main = do
    args <- getArgs
    parseFile args

parseFile :: [String] -> IO ()
parseFile [fileName] =
    withFile fileName ReadMode (\handle -> do
        contents <- BL.hGetContents handle
        let (cookie, order, left, right, dictWords) = G.runGet parseModel contents
            leftCount = length $ getChildren left
            rightCount = length $ getChildren right
        print (cookie, order, leftCount, rightCount, take 10 dictWords)
    )
parseFile _ = putStrLn "No filename specified."

parseModel :: G.Get (BL.ByteString, Word8, Tree, Tree, [BL.ByteString])
parseModel = do
    cookie <- G.getLazyByteString 9
    order <- G.getWord8
    leftTree <- parseTree
    rightTree <- parseTree
    dictLength <- G.getWord32le
    dictWords <- replicateM (fromIntegral dictLength) parseWord
    return (cookie, order, leftTree, rightTree, dictWords)

parseTree :: G.Get Tree
parseTree = do
    symbol <- G.getWord16le
    usage <- G.getWord32le
    count <- G.getWord16le
    branch <- G.getWord16le
    children <- replicateM (fromIntegral branch) parseTree
    return $ Tree symbol usage count children

parseWord :: G.Get BL.ByteString
parseWord = do
    wordLength <- G.getWord8
    word <- G.getLazyByteString $ fromIntegral wordLength
    return word
