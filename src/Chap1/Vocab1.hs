module Main where

import Data.Char (isLetter)
import Data.List (group, sort)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Control.Monad.Trans.State.Lazy

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
uniqueWithOrderSet = f S.empty []
 where
  f s acc [] = acc
  f s acc (x : xs)
    | S.member x s = f s acc xs
    | otherwise = f (S.insert x s) (x : acc) xs

-- keep appearence order, use Set, use Monad State
uniqueWithOrderSetState :: [T.Text] -> [T.Text]
uniqueWithOrderSetState = error "not implemented"

step :: S.Set T.Text -> (T.Text, S.Set T.Text)
step = undefined

stepS :: State (S.Set T.Text) T.Text
stepS = state step

--h :: [T.Text] -> State (S.Set T.Text) [T.Text]
--h = sequence t (m a)

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