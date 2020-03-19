{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- AnnotateColor: a module and CLI for
-- extracting color word data from text.
-- Part of my dissertation,
-- https://github.com/JonathanReeve/dissertation
-- All code licensed under the GPLv3.

module Main where

import Codec.Text.Detect (detectEncoding)
import Control.Applicative ((<|>), empty)
import Control.Monad (forM_)
import Data.Aeson
import Data.Attoparsec.Text as AT
import Data.Char
import Data.Function (on)
import Data.List (intersperse, sort, sortBy, sortOn, minimumBy)
import Data.Ord (comparing)
import Data.Maybe
import GHC.Generics
import Options.Generic
import System.FilePath
import qualified Clay as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Graphics.Plotly.Base as P

import Data.Colour.SRGB
import Data.Colour.CIE
import Data.Colour.RGBSpace.HSV
import Data.Colour.CIE
import Data.Colour.CIE.Illuminant (d65)
import Graphics.Color.Adaptation
import Graphics.Color.Model

import qualified ColorMaps as CM

-- import Frames
-- import Frames.CSV (readTableOpt, rowGen, RowGen(..))


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
      div_ [ class_ "container" ] $ do
        section_ [ class_ "section" ] $ do
          contents
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


-- -- | Make parent data for feeding to mkHBarTraces
-- -- mkParentData ::
-- mkParentData colorData colorMap = 

-- | CLI to annotate colors in text.
-- Usage: runhaskell AnnotateColor my-text-file.txt > out.html
main :: IO ()
main = do

   -- Process color map
   let mapName = CM.name CM.xkcd
   colorMap <- CM.assoc CM.xkcd
   -- Make Data.Map map out of it
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
   let zipData = map getZipData (zip (getLocations parsed) parsed)
   let onlyMatches = map fromJust $ filter isJust zipData
   let label = takeBaseName fileName
   let stats = [(makeStats (T.pack label) mapName (listToMap onlyMatches) colorMapMap)]
   print stats

   let outFileName = label ++ "-bar.html"
   -- [stats] for now, since we're making room for more of these later
   renderToFile outFileName $ scaffold $ do
     let childTraces = mkHBarTraces stats
     let parentTraces = (mkHBarParentTraces colorMapMap) stats
     let traces = concat [childTraces, parentTraces]
     let annotated = annotate colorMapMap parsed
     h1_ [] "Color Words in Aggregate"
     plotlyChart' traces "div1"
     h1_ [] "Annotated Text"
     div_ [ class_ "annotated" ] $ do
       toHtmlRaw annotated

   -- Output just the data.
   -- TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . encode $ stats
