{-# LANGUAGE OverloadedStrings #-}

module CategorizeColor where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.List (sortBy, sortOn, minimumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

import Data.Colour.SRGB
import Data.Colour.CIE (luminance, cieLABView)
import Data.Colour.RGBSpace.HSV
import Data.Colour.CIE.Illuminant (d65)

import Types

--  Calculate the distance to each base color, and find the
-- one with the smallest distance.
categorizeColor :: Hex -> ColorMap -> ColorWord
categorizeColor color colorMap = argMin deltas where
  -- TODO: maybe use Maybe here instead, just in case the base color isn't found in the map
  baseColorMap = [ (baseColor, fromMaybe "#ffffff" (colorMap M.!? baseColor)) | baseColor <- baseColors ]
  -- Make Colour objects for each hex
  baseColours :: [ (ColorWord, Colour Double) ]
  baseColours = map (\(cWord, cHex) -> (cWord, readColor cHex)) baseColorMap
  deltas :: [ (ColorWord, Double) ]
  deltas = [ (fst baseColor, deltaE76 (readColor color) (snd baseColor)) | baseColor <- baseColours ]
  argMin xs = fst $ minimumBy (comparing snd) xs

readColor :: Hex -> Colour Double
readColor hex = fst $ head $ sRGB24reads $ T.unpack hex

-- Given two colors in CIELAB color space, \( {L^*_1},{a^*_1},{b^*_1}) \)
-- and \( {L^*_2},{a^*_2},{b^*_2} \), the CIE76 color difference formula is defined as:
-- \[ \Delta E_{ab}^* = \sqrt{ (L^*_2-L^*_1)^2+(a^*_2-a^*_1)^2 + (b^*_2-b^*_1)^2 } \]
-- https://en.wikipedia.org/wiki/Color_difference
deltaE76 :: Colour Double -> Colour Double -> Double
deltaE76 color1 color2 = sqrt $ (l2-l1)**2 + (a2-a1)**2 + (b2-b1)**2 where
  (l1, a1, b1) = cieLABView d65 color1
  (l2, a2, b2) = cieLABView d65 color2

baseColors :: [ColorWord]
baseColors = ["black", "white", "grey", "red", "orange", "yellow", "green", "blue", "purple"]

sortColors :: (Colour Double -> Double) -> [(ColorWord, Hex, Parent, Int, [Span])] -> [(ColorWord, Hex, Parent, Int, [Span])]
sortColors selectionFunction colorStats = sortOn sortFunction colorStats where
  -- convert to HSL and get the hue to sort on.
  sortFunction (_, hex, _, _, _) = selectionFunction $ readColor hex
