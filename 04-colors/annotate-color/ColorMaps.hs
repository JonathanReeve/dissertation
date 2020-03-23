{-# LANGUAGE OverloadedStrings #-}

module ColorMaps where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map.Strict as M

import CategorizeColor (baseColors)

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

-- | Take a color map containing things like ("nile blue", "#4E5180")
--   and return ("nile", "#4E5180"), ("blue", "#4E5180")
extendMap :: [(ColorWord, Hex)] -> [(ColorWord, Hex)]
extendMap colorMap = concatMap split colorMap where
  split :: (ColorWord, Hex) -> [(ColorWord, Hex)]
  split (cw, hex) = map (\word -> (word, hex)) (T.words cw)

-- | An extended Ridgway, with base colors from XKCD,
-- since Ridgway bizarrely doesn't have base colors.
ridgwayXkcdMap = do
  rwMap <- M.fromList . extendMap <$> assoc ridgway
  xkcdMap <- M.fromList <$> assoc xkcd
  let xkcdBase = M.fromList [ (baseColor, (M.findWithDefault "#ffffff" baseColor xkcdMap))
                   | baseColor <- baseColors ]
  return (M.union xkcdBase rwMap)
  -- return $ [ M.insert (fst item) (snd item) rwMap | item <- xkcdBase ]
  -- return $ foldl (\item -> M.insert (fst item) (snd item) rwMap) xkcdBase

ridgwayExtendedXkcd = ColorMap { name = "RidgwayExtendedXKCD"
                               , assoc = M.toList <$> ridgwayXkcdMap
                               }

colorMaps = [xkcd, ridgway, ridgwayExtendedXkcd]
