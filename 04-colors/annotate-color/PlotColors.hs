{-# LANGUAGE OverloadedStrings #-}

module PlotColors where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Graphics.Plotly.Base as P
import Graphics.Plotly
import Graphics.Plotly.Lucid
import Lens.Micro
import Lucid
import qualified Statistics.Sample.Histogram as S
import qualified Data.Vector as V

import Types

-- | Let's do that again, but just take traces.
plotlyChart' :: [Trace] -> T.Text -> Html ()
plotlyChart' traces divName = toHtml $ plotly divName traces
                              & layout . barmode ?~ Stack

-- | Make traces from color data.
-- We need three traces here. Y is the same in all:
-- the name of the text.
-- X is a list with one value each [x]
-- name is the color name.
mkHBarTraces :: ColorStatsMap -> [Trace]
mkHBarTraces = Prelude.concatMap makeTraces where
  makeTraces :: (TextName, ColorMapName, [(ColorWord, Hex, Parent, Int, [Span])]) -> [Trace]
  makeTraces (textName, colorMapName, colorData) = map (makeTrace textName) colorData

mkHBarParentTraces :: ColorMap -> ColorStatsMap -> [Trace]
mkHBarParentTraces colorMap = Prelude.concatMap makeTraces where
  makeTraces :: (TextName, ColorMapName, [(ColorWord, Hex, Parent, Int, [Span])]) -> [Trace]
  makeTraces (textName, colorMapName, colorData) = map (makeTrace textName') colorData' where
    textName' = T.concat [textName, "-categories"]
    colorData' = map parentToColor colorData
    parentToColor (colorWord, hex, parent, n, spans) = (parent, colorMap M.! parent, "NAN", n, spans)

makeTrace :: TextName -> (ColorWord, Hex, Parent, Int, [Span]) -> Trace
makeTrace textName (colorWord, hex, _, n, _) = bars & P.y ?~ [toJSON textName]
                                                & P.x ?~ [toJSON n]
                                                & name ?~ colorWord
                                                & orientation ?~ Horizontal
                                                & marker ?~
                                                (defMarker & markercolor ?~ P.All (toJSON hex))


-- Plotly stacked and filled area plot. Let's make this JavaScript:
-- var plotDiv = document.getElementById('plot');
-- var traces = [
-- 	{x: [1,2,3], y: [2,1,4], stackgroup: 'one'},
-- 	{x: [1,2,3], y: [1,1,2], stackgroup: 'one'},
-- 	{x: [1,2,3], y: [3,0,2], stackgroup: 'one'}
-- ];

-- So we need Xs and Ys.
-- For each trace:
--   * Xs will be chunk indices (e.g. 1-10) and
--   * Ys will be a color and its values (name, color, y-value)

-- type ColorStatsMap = [(TextName, ColorMapName, [(ColorWord, Hex, Parent, Int, [Span])])]
mkChunkedTraces :: ColorStatsMap -> -- | Color statistics
                    Int ->  -- | Length of text
                    Int ->  -- | Number of desired chunks
                    [Trace]
mkChunkedTraces stats len nChunks = concatMap makeStat stats where
  makeStat (textName, cm, statsList) = map mkTrace statsList where
    mkTrace :: (ColorWord, Hex, Parent, Int, [Span]) -> Trace
    mkTrace (colorWord, hex, parent, count, spanList) =
      scatter & P.x     ?~ fmap toJSON [1..nChunks]
              & P.y     ?~ fmap toJSON yVals
              & name    ?~ colorWord
              & mode    ?~ [Lines]
              & marker ?~ (defMarker & markercolor ?~ P.All (toJSON hex))
              & stackgroup ?~ "one"
      where
      -- Make Y values, which are the number of times a color appears in a chunk.
      -- Ex: 0 1 2 0 0 0 10 2 0
      yVals = V.toList $ snd (S.histogram nChunks spanVec) :: [Int]
      starts = map (fromIntegral . fst) spanList :: [Double]
      spanVec = V.fromList starts :: V.Vector Double
