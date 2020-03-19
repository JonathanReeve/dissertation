{-# LANGUAGE OverloadedStrings #-}

module ColorMaps where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map.Strict as M

data ColorMap = ColorMap { name :: T.Text
                         , assoc :: IO [(ColorWord, Hex)]
                         }

type Hex = T.Text
type ColorWord = T.Text

parseTSV :: T.Text -> [(ColorWord, Hex)]
parseTSV tsv = sortBy (flip (compare `on` T.length . fst)) unsorted
  where
    textLines = tail $ T.lines tsv
    unsorted = [ mapTuple T.strip ( T.breakOn "\t" ln ) | ln <- textLines ]
    mapTuple f (a1, a2) = (f a1, f a2)

xkcd = ColorMap { name = "XKCD"
                , assoc = parseTSV <$> TIO.readFile "../data/maps/xkcd/rgb.txt"
                }

ridgway = ColorMap { name = "Ridgway"
                   , assoc = parseTSV <$> TIO.readFile "../data/maps/jaffer/ridgway.tsv"
                   }
