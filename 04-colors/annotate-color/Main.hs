{-# LANGUAGE OverloadedStrings #-}

-- AnnotateColor: a module and CLI for
-- extracting color word data from text.
-- Part of my dissertation,
-- https://github.com/JonathanReeve/dissertation
-- All code licensed under the GPLv3.

module Main where

import Codec.Text.Detect (detectEncoding)
import Control.Monad (forM_)
import Data.Attoparsec.Text as AT
import Data.Function (on)
import Data.List (intersperse, sort, sortBy, sortOn)
import Data.Maybe
import Options.Generic -- Command-line
import System.FilePath
import qualified Clay as C

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Lucid

import Graphics.Plotly.Lucid (plotlyCDN)

-- My own modules
import qualified ColorMaps as CM
import FindColors
import PlotColors
import AnnotateColors
import Types

-- | Make a standard HTML page from the results,
-- by scaffolding it with an HTML template.
scaffold :: Html () -> Html ()
scaffold contents =
  html_ [] $ do
    head_ [] $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      style_ [type_ "text/css"] $ C.render css
      plotlyCDN
      link_ [ rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css" ]
      link_ [ rel_ "stylesheet", href_ "https://fonts.googleapis.com/icon?family=Material+Icons" ]
    body_ $ do
      div_ [ class_ "container" ] $
        section_ [ class_ "section" ] contents
      scripts

scripts :: Html ()
scripts = mapM_ (\src -> with (script_ "") [ src_ src ]) [
  "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js"
  ]


-- | The styling for the result web page
css :: C.Css
css = "div.annotated" C.? do
         C.backgroundColor "#555"
         C.color "#ddd"


-- | CLI to annotate colors in text.
-- Usage: runhaskell AnnotateColor my-text-file.txt > out.html
main :: IO ()
main = do

   -- Parse command-line argument, and read the filename given
   -- by the first argument.
   fileName <- getRecord "Color word annotator."
   -- inFile <- TIO.readFile fileName
   rawByteStr <- B.readFile fileName

   -- Try to decode Utf-8 first, but if not, try Latin1.
   let inFile = case TE.decodeUtf8' rawByteStr of
                  Left err -> TE.decodeLatin1 rawByteStr
                  Right text -> text

   let label = takeBaseName fileName

   let cm = CM.xkcd
   colorMap <- CM.assoc cm
   let html = doAnalysis inFile label colorMap (CM.name cm)

   -- Write to a file.
   let outFileName = label ++ "-out.html"
   renderToFile outFileName html

   -- Output just the data.
   -- TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . encode $ stats

doAnalysis :: T.Text -> -- | Input file
             String -> -- | Input file label
             [(ColorWord, Hex)] -> -- | Color mapping
             T.Text -> -- | Color mapping label
             Html () -- | Resulting HTML
doAnalysis inFile label colorMap colorMapLabel = do
  -- let colorMap' = CM.extendMap colorMap
  let colorMapMap = M.fromList colorMap

  let parsed = findReplace (colorParser colorMap) inFile
  let zipData = map getZipData (zip (getLocations parsed) parsed)
  -- let onlyMatches = map fromJust $ filter isJust zipData
  let onlyMatches = catMaybes zipData
  let stats = makeStats (T.pack label) colorMapLabel (listToMap onlyMatches) colorMapMap
  mkHtml colorMapMap [stats] parsed (T.length inFile)

mkHtml :: ColorMap -> ColorStatsMap -> [ColorOrNot] -> Int -> Html ()
mkHtml colorMap stats parsed len = scaffold $ do
  h1_ [] "Color Words in Aggregate"
  let barTraces = (mkHBarTraces stats) ++ (mkHBarParentTraces colorMap stats)
  plotlyChart' barTraces "div1"
  h1_ [] "Color Words in Narrative Timeseries"
  let lineTraces = mkChunkedTraces stats len 10
  plotlyChart' lineTraces "div2"
  h1_ [] "Annotated Text"
  let annotated = annotate colorMap parsed
  div_ [ class_ "annotated" ] $ toHtmlRaw annotated
