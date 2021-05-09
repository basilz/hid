module Main where

import Control.Monad.Trans.State.Lazy
import Data.Char (isLetter)
import Data.List (group, sort)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

-- as in the book
uniqueWords :: [T.Text] -> [T.Text]
uniqueWords = map head . group . sort

-- use Set internally
uniqueWordsSet :: [T.Text] -> [T.Text]
uniqueWordsSet = S.toList . S.fromList

-- keep order of appearance, do not use Set
uniqueWithOrder :: [T.Text] -> [T.Text]
uniqueWithOrder = f []
 where
  f acc [] = acc
  f acc (x : xs)
    | x `elem` acc = f acc xs
    | otherwise = f (x : acc) xs

-- keep appearence order, use Set, no Monad State
uniqueWithOrderSet :: [T.Text] -> [T.Text]
uniqueWithOrderSet xs = fst $ foldl f ([], S.empty) xs
 where
  f (acc, s) w
    | S.member w s = (acc, s)
    | otherwise = (w : acc, S.insert w s)

-- keep appearence order, use Set, use Monad State
uniqueWithOrderSetState :: [T.Text] -> [T.Text]
uniqueWithOrderSetState xs = fst $ execState (traverse (modify . f) xs) ([], S.empty)
 where
  f w (acc, s)
    | S.member w s = (acc, s)
    | otherwise = (w : acc, S.insert w s)

main :: IO ()
main = do
  [fname] <- getArgs
  text <- TIO.readFile fname
  let ws =
        uniqueWithOrderSet $
          map T.toCaseFold $
            filter (not . T.null) $
              map (T.dropAround $ not . isLetter) $ T.words text
  TIO.putStrLn $ T.unwords ws
  print $ length ws