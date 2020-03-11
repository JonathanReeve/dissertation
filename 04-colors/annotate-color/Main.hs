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
import Data.Either
import Data.Function (on)
import Data.List (intersperse, sort, sortBy, sortOn, minimumBy)
import Data.Ord (comparing)
import Data.Maybe
import GHC.Generics
import Graphics.Plotly
import Graphics.Plotly.Lucid
import Lens.Micro
import Lucid
import Options.Generic
import Replace.Attoparsec.Text
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

-- import Frames
-- import Frames.CSV (readTableOpt, rowGen, RowGen(..))

-- | Just some useful type aliases here
type ColorWord = T.Text
type Hex = T.Text
type Parent = T.Text -- Category
type ColorMap = M.Map ColorWord Hex

type ColorOrNot = Either Unmatched (ColorFound, ColorStandardized)
type Unmatched = T.Text
type ColorFound = T.Text
type ColorStandardized = T.Text

type Span = (Start, End)
type Start = Int
type End = Int

type ColorStatsMap = [(TextName, ColorMapName, [(ColorWord, Hex, Parent, Int, [Span])])]
type TextName = T.Text
type ColorMapName = T.Text

data ColorStats = ColorStatsMap deriving (Generic, ToJSON, FromJSON)


-- * Parsing the colors

-- | Takes a list of words like "light green blue" and makes a
-- parser which will find "light green blue" and also "light green-blue",
-- "light green\nblue" and so on.
wordListParser :: [T.Text] -> Parser T.Text
wordListParser [w] = do -- One word case
  word <- asciiCI w
  return word
wordListParser (w:ws) = do  -- Multi-word case
  a <- asciiCI w    -- single word
  b <- space <|> char '-' -- followed by a separator
  c <- wordListParser ws          -- and more words
  -- Parse to space-separated.
  return (a `T.append` (T.singleton ' ') `T.append` c) -- singleton :: Char -> Text

-- | Parse word boundaries.
wordBoundary :: Parser Char
wordBoundary = space <|> satisfy isPunctuation

-- | Don't just parse the word, parse it with word boundaries
-- on either side.
withBoundaries :: Parser T.Text -> Parser T.Text
withBoundaries word = do
  wordBoundary
  w <- word
  wordBoundary
  return w

-- | Make one big parser out of our color map, and the expressions
-- generated from wordListParser.
colorParser :: [(ColorWord, Hex)] -> Parser T.Text
colorParser colorMap = choice $ map withBoundaries parsers where
  parsers = map (wordListParser . T.words . fst) $ colorMap

-- | Makes HTML from a color word and hex pair.
-- I.e. "red" -> "<span class="color" style="color: #ff0000">"
makeSpan :: T.Text -> T.Text -> TL.Text
makeSpan colorWord hex = TL.concat [" ", t, " "] where
  t = renderText $ span_ attrs (toHtml colorWord)
  attrs = [ class_ "color", style_ (T.concat ["color: ", hex])::Attribute ]

-- | Maps a function across both items in a tuple
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

-- * Text-to-color maps

-- | Processes the plain text (TSV) color map from XKCD,
-- and converts it to the list of tuples we'll be using for
-- a color map.
xkcdMap :: T.Text -> [(T.Text, T.Text)]
xkcdMap rawMap = reverse $ sortBy (compare `on` T.length . fst) unsorted
  where
    textLines = tail $ T.lines rawMap
    unsorted = [ mapTuple T.strip ( T.breakOn "\t" ln ) | ln <- textLines ]

-- * Annotate color words in text, using HTML
annotate :: ColorMap -> [ColorOrNot] -> T.Text
annotate colorMapMap results = T.concat $ map processBlock results where
  processBlock :: ColorOrNot -> T.Text
  processBlock block = case block of
    Left txt -> txt
    -- textFormat is the way the color expression is formatted in the text;
    -- stdFormat is the way it is formatted in the standardized way that it appears in the
    -- color map.
    Right (textFormat, stdFormat) ->
      -- Lowercase it first.
      let stdFormatLower = T.toLower stdFormat in
        case (colorMapMap M.!? stdFormatLower) of
          Nothing -> T.concat ["CANTFIND", stdFormat]
          Just hex -> TL.toStrict $ makeSpan textFormat hex

-- | Takes the parser output and makes spans
-- (start, end) for their locations
getLocations :: [ColorOrNot] -> [Span]
getLocations xs = zip <*> tail $ 0:(scanl1 (+) (getLengths xs)) where
  getLengths :: [ColorOrNot] -> [Int]
  getLengths xs = fmap getLength xs
  getLength x = case x of
    Left text -> T.length text
    Right (txtFormat, stdFormat) -> T.length txtFormat

-- | Actually do the replacement in the text.
findReplace :: Parser T.Text -> T.Text -> [ColorOrNot]
findReplace parser sourceText = fromRight [] $ parseOnly (findAllCap parser) sourceText

-- | Make a standard HTML page from the results,
-- by scaffolding it with an HTML template.
scaffold :: T.Text -> T.Text
scaffold text = TL.toStrict $ renderText $
  html_ [] $ do
    head_ [] $ do
      style_ [type_ "text/css"] $ C.render css
    body_ [] $ toHtmlRaw text

-- | The styling for the result web page
css :: C.Css
css = "body" C.? do
         C.backgroundColor "#333"
         C.color "#ddd"

-- | Utility for making printable data sets of the color name locations
-- so that they can be used in analysis later.
getZipData :: (Span, ColorOrNot) -> Maybe (Span, ColorFound, ColorStandardized)
getZipData (locs, parsed) = case parsed of
  Left _ -> Nothing
  Right (txtFormat, stdFormat) -> Just (locs, txtFormat, stdFormat)

-- | Utility to convert a list [("a", 2), ("a", 3), ("b", 2)] to a Map
-- like [("a", [2, 3]), "b", [2])]
listToMap :: [(Span, b, Text)] -> M.Map ColorWord [Span]
listToMap l = M.fromListWith (++) [(T.toLower stdFormat, [(loc1, loc2)]) |
                                   ((loc1, loc2), txtFormat, stdFormat) <- l]

makeStats :: TextName -> ColorMapName -> M.Map ColorWord [Span] -> ColorMap ->
  (TextName, ColorMapName, [(ColorWord, Hex, Parent, Int, [Span])])
makeStats fileName mapName locs colorMap = (fileName, mapName, stats ) where
  -- TODO: add more sort functions than just luminance.
  stats = (sortColors luminance) $ map makeStat (M.toList locs)
  makeStat (colorWord, spans) = (colorWord, hex, parent, length spans, spans) where
    hex = case colorMap M.!? colorWord of
      Nothing -> "UNDEFINED"
      Just hex -> hex
    parent = categorizeColor hex colorMap

--  Calculate the distance to each base color, and find the
-- one with the smallest distance.
categorizeColor :: Hex -> ColorMap -> ColorWord
categorizeColor color colorMap = argMin deltas where
  -- TODO: maybe use Maybe here instead, just in case the base color isn't found in the map
  baseColorMap = [ (baseColor, colorMap M.! baseColor) | baseColor <- baseColors ]
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
deltaE76 color1 color2 = sqrt $ dL**2 + da**2 + db**2 where
  (l1, a1, b1) = cieLABView d65 color1
  (l2, a2, b2) = cieLABView d65 color2
  dL = l2-l1
  da = a2-a1
  db = b2-b1

baseColors :: [ColorWord]
baseColors = ["black", "white", "grey", "red", "orange", "yellow", "green", "blue", "purple"]

sortColors :: (Colour Double -> Double) -> [(ColorWord, Hex, Parent, Int, [Span])] -> [(ColorWord, Hex, Parent, Int, [Span])]
sortColors selectionFunction colorStats = sortOn sortFunction colorStats where
  -- convert to HSL and get the hue to sort on.
  sortFunction (_, hex, _, _, _) = selectionFunction $ readColor hex

plotlyChart :: [ColorStatsMap] -> Html ()
plotlyChart colorData = mapM_ makeChart colorData where
  makeChart someData = toHtml $ plotly "div7" (mkHBarTraces someData)
                       & layout . margin ?~ thinMargins
                       & layout . height ?~ 300
                       & layout . width ?~ 800
                       & layout . barmode ?~ Stack

plotlyScaffold :: Html () -> Html ()
plotlyScaffold contents = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    plotlyCDN
  body_ $ contents

-- | Make traces from color data.
-- We need three traces here. Y is the same in all:
-- the name of the text.
-- X is a list with one value each [x]
-- name is the color name.
mkHBarTraces :: ColorStatsMap -> [Trace]
mkHBarTraces = Prelude.concatMap makeTraces where
  makeTraces :: (TextName, ColorMapName, [(ColorWord, Hex, Parent, Int, [Span])]) -> [Trace]
  makeTraces (textName, colorMapName, colorData) = map (makeTrace textName) colorData where
    makeTrace :: TextName -> (ColorWord, Hex, Parent, Int, [Span]) -> Trace
    makeTrace textName (colorWord, hex, _, n, _) = bars & P.y ?~ [toJSON textName]
                                                   & P.x ?~ [toJSON n]
                                                   & name ?~ colorWord
                                                   & orientation ?~ Horizontal
                                                   & marker ?~
                                                   (defMarker & markercolor ?~ P.All (toJSON hex))

-- | CLI to annotate colors in text.
-- Usage: runhaskell AnnotateColor my-text-file.txt > out.html
main :: IO ()
main = do
   -- Load color map. (Data from https://xkcd.com/color/rgb.txt )
   rawText <- TIO.readFile "../data/maps/xkcd/rgb.txt"
   -- Process color map
   let colorMap = xkcdMap rawText
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
   let mapName = "XKCD" :: Text
   let label = takeBaseName fileName
   let stats = [(makeStats (T.pack label) mapName (listToMap onlyMatches) colorMapMap)]
   print stats


   -- let outFileName = label ++ "-bar.html"
   -- -- [stats] for now, since we're making room for more of these later
   -- renderToFile outFileName $ plotlyScaffold $ plotlyChart [stats]

   -- Output just the data.
   -- TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . encode $ stats

   -- let annotated = annotate colorMapMap parsed
   -- let scaffolded = scaffold annotated

   -- -- Print the annotated version
   -- TIO.putStr $ scaffolded
