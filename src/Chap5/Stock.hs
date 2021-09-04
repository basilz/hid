{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Stock where

import Colonnade (Colonnade, Headed, ascii, headed)
import Control.Arrow ((&&&), (<<<))
import Control.Foldl (Fold (Fold))
import qualified Control.Foldl as L
import Control.Lens (Profunctor (lmap))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromField (..), FromRecord, HasHeader (HasHeader), ToField, ToRecord)
import Data.Csv.Streaming (Records (Cons, Nil), decode)
import Data.String (String)
import Data.Time (Day, UTCTime, defaultTimeLocale, diffDays, parseTimeM)
import Data.Time.Clock (UTCTime (UTCTime))
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy hiding (Fold, close)
import Numeric (showFFloat)
import Protolude hiding (ByteString)
import Streaming
import qualified Streaming.Prelude as S

------------ logic -------------------------------------

instance FromField Day where
  parseField r = parseTimeM True defaultTimeLocale "%F" $ toS $ decodeUtf8 r

data Row = Row
  { day :: Day
  , close :: Double
  , volume :: Integer
  , open :: Double
  , high :: Double
  , low :: Double
  }
  deriving (Generic, FromRecord, Show)

-- folding in constant space works if the state is strict
data Feature = Feature
  { feature_name :: !String
  , feature_mean :: !Double
  , feature_min :: !Double
  , feature_max :: !Double
  , feature_days :: !Integer
  }
  deriving (Show, Generic, ToRecord)

-- per feature Fold applicative composition
feature :: String -> (Row -> Double) -> Fold Row (Maybe Feature)
feature feature_name f = rmap -- fmap
  do
    \case
      (Nothing, _, _) -> Nothing --  *lazy tuple, thank you avoiding me to fix L.mean
      (Just (feature_min, dmin), Just (feature_max, dmax), feature_mean) -> Just $ Feature {..}
        where
          feature_days = abs $ diffDays dmin dmax
  do
    (,,)
      <$> lmap (f &&& day) L.minimum
      <*> lmap (f &&& day) L.maximum
      <*> lmap f L.mean -- unsafe for empty input, but see the *lazy tuple

-- exercise write safeMean :: Fractional a => Fold a (Maybe a)

-- per field applicative composition
reports :: Fold Row (Maybe [Feature])
reports =
  sequence
    <$> sequenceA do
      [ feature "open" open
        , feature "close" close
        , feature "high" high
        , feature "low" low
        , feature "Integer" $ fromIntegral . volume
        ]

-- caveman, forgets all problems (S.fold_ + S.concat )
runReports :: Monad m => Stream (Of (Either String Row)) m r -> m [Feature]
runReports = fmap (join . maybeToList) . L.purely S.fold_ reports . S.concat

---- rendering -----------------

colFeature :: Colonnade Headed Feature String
colFeature =
  mconcat
    [ headed "Feature" feature_name
    , headed "Mean" $ showF . feature_mean
    , headed "Min" $ showF . feature_min
    , headed "Max" $ showF . feature_max
    , headed "Days" $ showF . fromIntegral . feature_days
    ]

showF :: Double -> String
showF v = showFFloat (Just 2) v ""

-------- run ---------------------
streamCSV :: FilePath -> Stream (Of (Either String Row)) IO (Maybe (String, ByteString))
streamCSV dataPath = do
  liftIO (decode HasHeader <$> BL.readFile do dataPath) >>= fix do
    \go -> \case
      Cons x r -> S.yield x >> go r
      Nil Nothing "" -> pure Nothing
      Nil (Just e) b -> pure $ Just (e, b)

dataPath :: FilePath
dataPath = "data/quotes.csv"

main :: IO ()
main = runReports (streamCSV dataPath) >>= putStrLn . ascii colFeature

graph :: IO ()
graph = do
  rows <- rights <$> S.toList_ (streamCSV dataPath)
  toFile def "out/quotes.svg" $ do
    layout_title .= "quote data"
    layout_plots .= fmap
      do toPlot . lineOf rows
      do
        [ ("open", open, green)
          , ("close", close, blue)
          , ("high", high, red)
          , ("low", low, gray)
          ]

lineOf :: [Row] -> (String, Row -> y, Colour Double) -> PlotLines Day y
lineOf vs (t, f, c) =
  def
    & plot_lines_style . line_color .~ opaque c
    & plot_lines_values .~ [(day &&& f) <$> vs]
    & plot_lines_title .~ t
