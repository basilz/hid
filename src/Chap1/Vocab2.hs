{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char (isLetter)
import Data.Function ((&))
import Data.List (group, sort)
import qualified Data.Text as T 
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Protolude
import qualified Data.Map.Strict as M

type Entry = (T.Text, Int)
type Vocabulary = [Entry]

-- use a Map Text Int to accumulate
extractVocabMap :: [T.Text] -> Vocabulary -- [(Text,Int)]
extractVocabMap xs = M.assocs $ foldl' (&) mempty $ M.unionWith (+) <$> ((`M.singleton` 1) <$> xs)

extractVocabMap' :: [T.Text] -> Vocabulary -- [(Text,Int)]
extractVocabMap' xs = M.assocs $ foldr' ($) mempty $ M.unionWith (+) <$> ((`M.singleton` 1) <$> xs)

f :: (Foldable t, Functor t, Monoid a, Num a) => t a -> a
f xs = foldl (&) mempty $ (+) <$> xs


--exctractVocabMapNormalPeople :: (Foldable t, Ord k, Num a) => t k -> [(k, a)]
--exctractVocabMapNormalPeople = M.assocs . foldMap (`M.singleton` 1)

extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
  where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    buildEntry xs@(x : _) = (x, length xs)
    cleanWord = T.dropAround (not . isLetter)

extractWords :: T.Text -> [T.Text]
extractWords t = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
  where
    cleanWord = T.dropAround (not . isLetter)

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
    putStrLn "All words: "
    TIO.putStrLn $ T.unlines $ map fst vocab

processTextFile :: FilePath -> IO ()
processTextFile fname = do
    text <- TIO.readFile fname
    let vocab = extractVocabMap . extractWords $ text
--    print vocab
    printAllWords vocab

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fname] -> processTextFile fname
        _ -> putStrLn "Usage: vocab-builder filename"