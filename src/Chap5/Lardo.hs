
{-# LANGUAGE DeriveFunctor #-}

module Chap5.Lardo where

import Control.Lens (Profunctor (..))
import Protolude (notImplemented)

newtype Lardo f a b = Lardo (a -> f b) deriving (Functor)

mkLardo :: Applicative f => (a -> b) -> Lardo f a b
mkLardo h = Lardo $ pure <$> h

instance Monoid (f b) => Monoid (Lardo f a b) where
  mempty = Lardo mempty 

instance Semigroup (f b) => Semigroup (Lardo f a b) where
  Lardo f <> Lardo g = Lardo $ (<>) <$> f <*> g

instance Functor f => Profunctor (Lardo f) where
  lmap f (Lardo g) = Lardo $ g . f
  rmap f lg = f <$> lg
