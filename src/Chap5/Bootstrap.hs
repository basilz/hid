module Chap5.BeautifulFold where

import qualified Control.Foldl as F
import qualified Data.Map as M
import qualified Debug.Trace as T

type Price = Double
type Maturity = Double
type Yield = Double

data Bond = Bond Maturity Price

newtype Curve = Curve [(Maturity, Yield)] deriving Show

yield :: Price -> Yield -> Maturity -> Maturity -> Yield
yield p pY t pT = T.trace ("yield of: " ++ show p ++ ", " ++ show pY ++ ", " ++ show t ++ ", " ++ show pT) $ - log (p / exp (-pY * pT)) / (t - pT)

join :: Curve -> Bond -> Curve
join (Curve []) (Bond t p) = Curve [(t, - log p / t)]
join (Curve ys@(x : y : _)) (Bond t p) =
    let (pT, pY) = x
        (ppT, ppY) = y
     in Curve ((t, yield p pY t pT):ys)
join (Curve ys@(x : _)) (Bond t p) =
    let (pT, pY) = x
     in Curve ((t, yield p pY t pT):ys)


bootstrap :: F.Fold Bond Curve
bootstrap = F.Fold join (Curve []) id




