{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Mealy where

import Control.Arrow (Arrow (arr, first), (&&&))
import Control.Category (Category (..))
import Control.Comonad
import Control.Foldl (Fold (Fold))
import qualified Control.Foldl as L
import qualified GHC.List as G
import Protolude hiding (first, (.))

{-
-- encoding of a mealy machine, notice the difference with Fold, a moore machine

data Fold a b  where
  Fold :: (x -> a -> x) -> x -> (x -> b) -> Fold a b

with Fold we can extract 'b' without applying 'a', Pointed instance , moore observability
OTOH vertical composition is hard, in particular the Category instance is probably not sound
(to be proved)
Vertical composition will make them act as one while the output of one goes into the input of the other one
We would like to compose
> Fold b c -> Fold a b -> Fold a c
to do that we introduce another machine Mealy (Scan in foldl package) that produces 'b' only when fed with an 'a', non observability of mealy
Mealy will compose better giving a 'c' every 'a'

-}
data Mealy a b where
    Mealy :: (x -> a -> (x, b)) -> x -> Mealy a b

deriving instance Functor (Mealy a)

-- Mealy still have lateral composition via applicative
instance Applicative (Mealy a) where
    pure :: b -> Mealy a b
    pure b = Mealy
        do \x -> const (x, b)
        do ()
    (<*>) :: Mealy a (b -> c) -> Mealy a b -> Mealy a c
    Mealy f x <*> Mealy g y = Mealy
        do
            \(xf, xg) a ->
                let (x', fbc) = f xf a
                    (x'', b) = g xg a
                 in ((x', x''), fbc b)
        do (x, y)

-- going from moore to mealy is easy ,
-- 1 . we lose observability on the first state
-- 2 . we use the observing function to output 'b' from x at every 'a'
mealy :: Fold a b -> Mealy a b
mealy (Fold f x g) = Mealy
    do \xf -> const (xf, g xf)
    do x

-- hard: implement *mealy* function without matching the Fold constructor
-- moore machines are comonads su we do not need to desctruture them here
mealy2 :: Fold a b -> Mealy a b
mealy2 m = Mealy
    do notImplemented
    do m

-- going from mealy to moore wants back the initial observation
-- we could have an isomorphism if *mealy* was
-- > mealy :: Fold a b -> (Mealy a b, b)
-- then
-- > uncurry moore . mealy == identity
moore :: Mealy a b -> b -> Fold a b
moore (Mealy f x) b = Fold
    do f . fst
    do (x, b)
    do snd

-- vertical composition for mealy, very similar to the applicative but with result threading
instance Category Mealy where
    (.) :: Mealy b c -> Mealy a b -> Mealy a c
    Mealy f x . Mealy g y = Mealy
        do
            \(xf, xg) a ->
                let (x', b') = g xg a
                    (x'', b'') = f xf b'
                 in ((x'', xg), b'')
        do (x, y)
    id = Mealy (\_ a -> ((), a)) ()

-- and now we can compose Fold vertically buy lifting them to Mealy
minOf :: Ord b => Fold a b -> Fold a (Maybe b)
minOf f = moore
    do mealy L.minimum . mealy f
    do Nothing

-- finally the parallel composition which is a superset of the applicative one
instance Arrow Mealy where
    arr :: (a -> b) -> Mealy a b
    arr f = Mealy
        do \x a -> ((), f a)
        do ()
    first :: Mealy a b -> Mealy (a, c) (b, c)
    first (Mealy f x) = Mealy
        do
            \x (a, c) ->
                let (x', b) = f x a
                 in (x, (b, c))
        do x

-- and now we can use arrows to freely compose Folds after lifting them to Mealys
-- in this case we want to compute min and max of every step of a fold
-- we can write this with applicative or arrow
minAndMaxOf :: Ord b => Fold a b -> Fold a (Maybe (b, b))
minAndMaxOf f = moore
    do mealy (liftA2 (,) <$> L.minimum <*> L.maximum) . mealy f
    do Nothing

-- final: make an example of something you cannot do with applicative but you can with arrow

-- esercizio su comonad fold. Scrivere blend xs ys zs =  (somma (xs <> zs), media (ys <> zs)) consumando zs una sola volta
blend :: [Double] -> [Double] -> [Double] -> (Double, Double)
blend xs ys = L.fold $ (,) <$> extend (`L.fold` xs) L.sum <*> extend (`L.fold` ys) L.mean

-- invece del b finale , un b ogni n  a
--sample :: Int -> Fold a b -> [a] -> [b]
--sample n f xs = L.fold (extend (\f' -> _ xs) f) xs

sample :: Int -> Fold a b -> [a] -> [b]
sample n = go
  where
    go _ [] = []
    go x ys =
        let (zs, ys') = splitAt n ys
            x' = x =>> flip L.fold zs
         in extract x' : go x' ys'

-- buon giorno, un altro esercizio dove invece penso sia necessario costruire il Fold col suo costruttore
-- pero' non serve smontare l'argomento, grazie alla comonade
sampleT :: Int -> Fold a b -> Fold a [b]
sampleT n fld = Fold step begin done
  where
    step (f, bs) a =
        let f' = extend (flip L.fold [a]) f
            (x, k) = extract f'
         in (f', if k `mod` n == 0 then (++ [x]) . bs else bs)
    begin = ((,) <$> fld <*> L.length, id)
    done (_, bs) = bs []

allTheSameM :: (Eq a, Monad m) => [m a] -> m Bool
allTheSameM ms = and . (zipWith (==) <*> G.tail) <$> sequence ms
