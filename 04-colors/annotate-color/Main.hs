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
import Web


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