{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MooreComonad where

import Control.Comonad (Comonad (duplicate, extend, extract))
import Control.Monad (guard)
import Debug.Trace
import Test.Hspec (hspec, it, shouldBe)

data Moore a b = Moore b (a -> Moore a b) deriving (Functor)

instance Comonad (Moore a) where
    extract (Moore b _) = b
    duplicate :: forall b. Moore a b -> Moore a (Moore a b)
    duplicate (Moore b f) = Moore (Moore b f) x
      where
        x :: a -> Moore a (Moore a b)
        x a = duplicate $ f a

slidingWindow :: Int -> Moore Double (Maybe Double)
slidingWindow i = Moore Nothing $ f []
  where
    f :: [Double] -> Double -> Moore Double (Maybe Double)
    f xs x = let xs' = take i (x : xs) in Moore (g xs') $ f xs'
    g :: [Double] -> Maybe Double
    g xs = do
        guard $ length xs == i
        return $ sum xs / fromIntegral i

-- step :: a -> Moore a b -> Moore a b
-- step a (Moore _ f) = f a
--
-- run :: [a] -> Moore a b -> Moore a b
-- run xs m = foldl (flip step) m xs

step :: a -> Moore a b -> b
step a (Moore _ f) = extract $ f a

-- foldr: extend (step x1) (extend (step x2) (extend (step x3) m))
-- foldl: extend (step x3) (extend (step x2) (extend (step x1) m))
run :: Show a => [a] -> Moore a b -> Moore a b
run xs m = foldr (extend . step) m xs

test :: IO ()
test = hspec $ do
    it "TEST" $
        extract (slidingWindow 10) `shouldBe` Nothing
    it "Feed 9" $
        extract (run [1 .. 9] $ slidingWindow 10) `shouldBe` Nothing
    it "Feed 10" $
        extract (run [1 .. 10] $ slidingWindow 10) `shouldBe` Just (10 * 11 / 20)
    it "Feed 11" $
        extract (run [1 .. 11] $ slidingWindow 10) `shouldBe` Just 6.5
    it "Feed 100" $
        extract (run [1 .. 100] $ slidingWindow 10) `shouldBe` Just 95.5
