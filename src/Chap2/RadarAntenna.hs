{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.Monad (replicateM, unless)
import Data.List (nub, sort)
import Fmt (
    Buildable (..),
    fmt,
    fmtLn,
    nameF,
    unwordsF,
    (+||),
    (||+),
 )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random (Random, getStdRandom, uniform)
import System.Random.Internal (
    Uniform (uniformM),
    UniformRange (uniformRM),
 )

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d
        | d == minBound = maxBound
        | otherwise = pred d

    csucc :: a -> a
    csucc d
        | d == maxBound = minBound
        | otherwise = succ d

data Direction = North | East | South | West
    deriving (Eq, Enum, Bounded, Show)

instance CyclicEnum Direction

data Turn = TNone | TLeft | TRight | TAround
    deriving (Eq, Enum, Bounded, Show)

instance Semigroup Turn where
    TNone <> t = t
    TLeft <> TLeft = TAround
    TLeft <> TRight = TNone
    TLeft <> TAround = TRight
    TRight <> TRight = TAround
    TRight <> TAround = TLeft
    TAround <> TAround = TNone
    t1 <> t2 = t2 <> t1

instance Monoid Turn where
    mempty = TNone

deriving instance Read Direction
deriving instance Read Turn
deriving instance Ord Turn

instance Buildable Direction where
    build North = "N"
    build East = "E"
    build South = "S"
    build West = "W"

instance Buildable Turn where
    build TNone = "--"
    build TLeft = "<-"
    build TRight = "->"
    build TAround = "||"

instance UniformRange Direction where
    uniformRM (lo, hi) rng = do
        res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
        pure $ toEnum res

instance Uniform Direction where
    uniformM rng = uniformRM (minBound, maxBound) rng

instance UniformRange Turn where
    uniformRM (lo, hi) rng = do
        res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
        pure $ toEnum res

instance Uniform Turn where
    uniformM rng = uniformRM (minBound, maxBound) rng

{-
Basic operations over a radar includes the following:

- rotate—determine a new antenna direction after rotating
- orient—find a rotation to change an orientation from the first given direction to the second one

As usual, we describe these operations with their type signatures:
-}

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = csucc . csucc

every :: (Bounded a, Enum a) => [a]
every = enumFrom minBound

everyDirection :: [Direction]
everyDirection = enumFrom minBound

orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

--Once we’ve implemented them, we can define functions over lists:

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate)

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl $ flip rotate

orientMany :: [Direction] -> [Turn]
orientMany ds@(_ : _ : _) = zipWith orient ds (tail ds)
orientMany _ = []

-- We are going to end up processing files and the main function:

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir fName = do
    f <- readFile fName
    let turns = map read $ lines f
        finalDir = rotateMany dir turns
        directions = rotateManySteps dir turns
    fmtLn $ "Final direction: " +|| finalDir ||+ ""
    fmt $ nameF "Intermediate directions" (unwordsF directions)

orientFromFile :: FilePath -> IO ()
orientFromFile fName = do
    f <- readFile fName
    let directions = map read $ lines f
        turns = orientMany directions
    fmt $ nameF "All turns: " (unwordsF turns)

-- main :: IO ()
-- main = do
--     args <- getArgs
--     case args of
--         ["-r", fName, dir] -> rotateFromFile (read dir) fName
--         ["-o", fName] -> orientFromFile fName
--         _ ->
--             putStr $
--                 "Usage: locator -o filename"
--                     <> "       locator -r filename direction"

-- For testing
uniformIO :: Uniform a => IO a
uniformIO = getStdRandom uniform

uniformsIO :: Uniform a => Int -> IO [a]
uniformsIO n = replicateM n uniformIO

randomTurns :: Int -> IO [Turn]
randomTurns = uniformsIO

randomDirections :: Int -> IO [Direction]
randomDirections = uniformsIO

writeRandomFile :: (Random a, Show a) => Int -> (Int -> IO [a]) -> FilePath -> IO ()
writeRandomFile n gen fname = do
    xs <- gen n
    writeFile fname $ unlines $ map show xs

test_allTurnsInUse :: Bool
test_allTurnsInUse = sort (nub [orient d1 d2 | d1 <- every, d2 <- every]) == every

test_rotationsMonoidAgree :: [Turn] -> Bool
test_rotationsMonoidAgree ts = and [rotateMany d ts == rotateMany d ts | d <- every]

test_orientRotateAgree :: [Direction] -> Bool
test_orientRotateAgree [] = True
test_orientRotateAgree ds@(d : _) = ds == rotateManySteps d (orientMany ds)

main :: IO ()
main = do
    ds <- randomDirections 1000
    ts <- randomTurns 1000
    unless
        ( test_allTurnsInUse
            && test_orientRotateAgree ds
            && test_rotationsMonoidAgree ts
        )
        exitFailure