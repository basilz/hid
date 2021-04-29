{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.FingerTree
    ( (><), split, viewl, (|>), FingerTree, Measured(..), ViewL((:<)) )
import Protolude
    ( ($),
      Eq,
      Num,
      Ord((>=), (>)),
      Show,
      Foldable(toList),
      Monoid(mempty),
      Int,
      Maybe(Just),
      IO,
      (.),
      Last(Last) )
import Test.Hspec (describe, hspec, it, shouldBe)

type Ordered a = FingerTree (Last a) a

-- extract 'k' highest scoring Song , O (1) * k
insert :: (Measured (Last a) a, Ord a) => a -> Ordered a -> Ordered a
insert x xs =
  let (l, r) = split (> Last (Just x)) xs
   in (l |> x) >< r

remove :: Show a => (Measured (Last a) a, Ord a) => a -> Ordered a -> Ordered a
remove x xs =
  let (l, r) = split (>= Last (Just x)) xs
      (l', r') = split (> Last (Just x)) r
  in case viewl l' of
     _ :< x'' -> l >< x'' >< r' 
     _ -> l >< r'

newtype TestInt = TestInt Int deriving (Num, Show, Eq, Ord)

instance Measured (Last TestInt) TestInt where
  measure = Last . Just

main :: IO ()
main = hspec do
  describe "insert" do
    it "add an element" do
      toList (insert 1 mempty) `shouldBe` [1 :: TestInt]
    it "add 2 elements" do
      shouldBe
        do toList $ insert 2 $ insert 1 mempty
        do [1, 2 :: TestInt]
    it "add 2 elements reversed sorting" do
      shouldBe
        do toList $ insert 1 $ insert 2 mempty
        do [1, 2 :: TestInt]
    it "add 3 elements with repetition" do
      shouldBe
        do toList $ insert 2 $ insert 1 $ insert 2 mempty
        do [1, 2, 2 :: TestInt]
    it "add 4 elements with repetition" do
      shouldBe
        do toList $ insert 0 $ insert 2 $ insert 1 $ insert 2 mempty
        do [0, 1, 2, 2 :: TestInt]
  describe "remove" do
    it "delete an element" do
      toList (remove 1 $ insert 1 mempty) `shouldBe` ([] :: [TestInt])
    it "delete 1 element" do
      shouldBe
        do toList $ remove 1 $ insert 2 $ insert 1 mempty
        do [2 :: TestInt]
    it "delete 1 element reversed sorting" do
      shouldBe
        do toList $ remove 2 $ insert 1 $ insert 2 mempty
        do [1 :: TestInt]
    it "remove 2 elements with repetition" do
      shouldBe
        do toList $ remove 1 $ remove 2 $ insert 2 $ insert 1 $ insert 2 mempty
        do [2 :: TestInt]
    it "remove 3 elements with repetition" do
      shouldBe
        do toList $ remove 2 $ insert 0 $ insert 2 $ remove 2 $ insert 1 $ insert 2 mempty
        do [0, 1 :: TestInt]
