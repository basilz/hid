{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.FingerTree
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Protolude
import Test.Hspec (describe, hspec, it, shouldBe)

-- how many times a song has been voted
newtype Score = Score Int
  deriving (Ord, Eq, Show, Bounded, Num)

-- song name
newtype Song = Song Text deriving (Eq, Ord, Show, IsString)

-- a song together with its score
data Hit = Hit
  { song :: Song
  , score :: Score
  }
  deriving (Eq, Show)

-- Hit ordering
type SortHit = (Down Song, Score)

-- project Hit into it's natural ordering
sortHit :: Hit -> SortHit
sortHit (Hit song score) = (Down song, score)

-- open and close Last

-- how to measure a Hit in a fingertree sense, checkout the measure must be a monoid
instance Measured (Last SortHit) Hit where
  measure (Just . sortHit -> h) = Last h

-- a fingertree where the monoid is an Last
type Ordered b a = FingerTree (Last b) a

-- song scores together with a fingertree for logarithmic time operations
data HitParade = HitParade
  { scores :: Map Song Score
  , parade :: Ordered SortHit Hit
  }
  deriving (Show)

noParade :: HitParade
noParade = HitParade mempty mempty

-- here is the plan where  n == number of songs
-- 1) get the song score , O (log n)
-- 2) remove the Hit (song + score) from the fingertree, O(log n)
-- 3) update the hit score  and insert it back into the fingertree, O(log n)
-- 4) update the scores , O (log n)
addVote :: Song -> HitParade -> HitParade
addVote song HitParade {..}  = 
  let scr = fromMaybe 0 $ M.lookup song scores
      (l, r) = split (>= Last (Just . sortHit $ Hit song scr)) parade
      (l', r') = split (> Last (Just . sortHit $ Hit song scr)) r
   in case viewl l' of
      Hit sng sc :< _ -> HitParade (M.update (\s -> Just $ s + 1) song scores) ((l |> Hit sng (sc + 1)) >< r')
      _ -> HitParade (M.insert song 1 scores) (l >< (Hit song 1 <| r))

-- extract 'k' highest scoring Song , O (1) * k
hits :: Int -> HitParade -> [Hit]
hits n (HitParade _ parade) = take n $ toList parade

main :: IO ()
main = hspec do
  describe "hits" do
    it "handles 1 votes" do
      shouldBe
        do hits 10 (addVote "route 66" noParade)
        do [Hit "route 66" 1]
    it "handles 2 votes for same song" do
      shouldBe
        do hits 10 (addVote "route 66" $ addVote "route 66" noParade)
        do [Hit "route 66" 2]
    it "handles 2 votes for different song, lexicographically swapped" do
      shouldBe
        do hits 10 (addVote "message in a bottle" $ addVote "route 66" noParade)
        do [Hit "route 66" 1, Hit "message in a bottle" 1]
    it "handles 2 votes for different song, lexicographically straight" do
      shouldBe
        do hits 10 (addVote "route 66" $ addVote "message in a bottle" noParade)
        do [Hit "route 66" 1, Hit "message in a bottle" 1]
    it "handles 3 votes for 2 different songs" do
      shouldBe
        do hits 10 (addVote "route 66" $ addVote "route 66" $ addVote "message in a bottle" noParade)
        do [Hit "route 66" 2, Hit "message in a bottle" 1]
    it "handles 3 votes for 2 different songs lexicographically swapped" do
      shouldBe
        do hits 10 (addVote "message in a bottle" $ addVote "message in a bottle" $ addVote "route 66" noParade)
        do [Hit "route 66" 1, Hit "message in a bottle" 2]
    it "handles 3 votes for 2 different songs another permutation" do
      shouldBe
        do hits 10 (addVote "route 66" $ addVote "message in a bottle" $ addVote "route 66" noParade)
        do [Hit "route 66" 2, Hit "message in a bottle" 1]
    it "handles 3 votes for 2 different songs still another permutation" do
      shouldBe
        do hits 10 (addVote "message in a bottle" $ addVote "route 66" $ addVote "message in a bottle" noParade)
        do [Hit "route 66" 1, Hit "message in a bottle" 2]

