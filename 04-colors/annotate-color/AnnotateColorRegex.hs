{-# LANGUAGE OverloadedStrings #-}

import Data.Text.ICU.Replace
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (reverse, sort, sortBy)
import Data.Function (on)
import Options.Generic

type Hex = T.Text
type ColorWord = T.Text
type Pat = T.Text

xkcdMap :: T.Text -> [(ColorWord, Hex)]
xkcdMap rawMap = reverse $ sortBy (compare `on` T.length . fst) unsorted
  where
    textLines = tail $ T.lines rawMap
    unsorted = [ mapTuple T.strip ( T.breakOn "\t" ln ) | ln <- textLines ]
    mapTuple f (a1, a2) = (f a1, f a2)

makePat :: (ColorWord, Hex) -> (ColorWord, Hex, Pat)
makePat (cw, hex) = (cw, hex, pat) where
  pat = T.concat ["\\b", T.intercalate "[ -\t\n\r]" (T.splitOn " " cw), "\\b"]

main :: IO ()
main = do
  -- Load color map. (Data from https://xkcd.com/color/rgb.txt )
  rawText <- TIO.readFile "../data/xkcd/rgb.txt"
    -- Process color map
  let colorMap = xkcdMap rawText

  -- Parse command-line argument, and read the filename given
  -- by the first argument.
  fileName <- getRecord "Color word annotator."
  inFile <- TIO.readFile fileName

  let colorMapPats = map makePat colorMap

  replaceAll pat repl inFile
  print colorMapPats
