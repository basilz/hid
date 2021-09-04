module MC where

import qualified Control.Foldl as F
import Data.Random.Normal

type Strike = Double
type Expiry = Double
type DiscountRate = Double
type Volatility = Double
type Spot = Double

data Option = Option Spot Strike Expiry

avg :: Fractional a => F.Fold a a
avg = (/) <$> F.sum <*> F.genericLength 

mc :: Option -> DiscountRate -> Volatility -> Int -> Double 
mc (Option spot k t) r vol n = F.fold (F.premap f avg) $ take n (mkNormals 42)
    where
        f x = spot * exp ((r - 0.5 * vol ^ 2) * t + x * vol * sqrt t)



main :: IO ()
main = undefined