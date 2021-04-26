{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Data.Char
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import Fmt
import qualified Fmt as F
import Control.Monad

type Entry = (T.Text, Int)
type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
  where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    buildEntry xs@(x:_) = (x, length xs)
    cleanWord = T.dropAround (not . isLetter)

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortOn (Down . snd)

wordsCount :: Vocabulary -> (Int, Int)
wordsCount vcb = (length $ allWords vcb, length $ wordsByFrequency vcb)

allWords :: Vocabulary -> [T.Text]
allWords = fmap fst 

allWordsReport :: Vocabulary -> T.Text
allWordsReport vocab =
  fmt $ nameF "All words" $ F.unlinesF (allWords vocab)

wordsCountReport :: Vocabulary -> T.Text
wordsCountReport vocab = fmt $
     "Total number of words: " +|total|+
     "\nNumber of unique words: " +|unique|+ "\n"
  where
    (total, unique) = wordsCount vocab

frequentWordsReport :: Vocabulary -> Int -> T.Text
frequentWordsReport vocab num =
    fmt $ nameF "Frequent words"
        $ blockListF' "" fmtEntry reportData
  where
    reportData = take num $ wordsByFrequency vocab
    fmtEntry (t, n) = ""+|t|+": "+|n|+""

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile fname withAllWords n = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  when withAllWords $ TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-a", fname, num] ->
      processTextFile fname True (read num)
    [fname, num] ->
      processTextFile fname False (read num)
    _ -> putStrLn "Usage: vocab3 [-a] filename freq_words_num"