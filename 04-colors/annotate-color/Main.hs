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
   -- Process color map
   let cm = CM.ridgway

   colorMap <- CM.assoc cm
   let colorMapMap = M.fromList colorMap

   -- Parse command-line argument, and read the filename given
   -- by the first argument.
   fileName <- getRecord "Color word annotator."
   -- inFile <- TIO.readFile fileName
   rawByteStr <- B.readFile fileName

   -- Try to decode Utf-8 first, but if not, try Latin1.
   let inFile = case TE.decodeUtf8' rawByteStr of
                  Left err -> TE.decodeLatin1 rawByteStr
                  Right text -> text

   -- Parse out the colors, get their locations
   let parsed = findReplace (colorParser colorMap) inFile
   -- Hlint suggests this the following, but I'm not so sure it's clearer.
   -- let zipData = zipWith (curry getZipData) (getLocations parsed) parsed
   let zipData = map getZipData (zip (getLocations parsed) parsed)
   -- let onlyMatches = map fromJust $ filter isJust zipData
   let onlyMatches = catMaybes zipData
   let label = takeBaseName fileName
   let stats = [makeStats (T.pack label) (CM.name cm) (listToMap onlyMatches) colorMapMap]
   print stats

   let outFileName = label ++ "-bar.html"

   renderToFile outFileName $ scaffold $ do
     let traces = (mkHBarTraces stats) ++ (mkHBarParentTraces colorMapMap stats)
     let annotated = annotate colorMapMap parsed
     h1_ [] "Color Words in Aggregate"
     plotlyChart' traces "div1"
     h1_ [] "Annotated Text"
     div_ [ class_ "annotated" ] $ toHtmlRaw annotated

   -- Output just the data.
   -- TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . encode $ stats
