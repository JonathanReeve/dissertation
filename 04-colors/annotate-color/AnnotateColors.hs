
-- import Main
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as TL
import Data.Attoparsec.Text as AT
import Replace.Attoparsec.Text
import Lucid
import Data.Either

import Types

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

-- | Makes HTML from a color word and hex pair.
-- I.e. "red" -> "<span class="color" style="color: #ff0000">"
makeSpan :: T.Text -> T.Text -> TL.Text
makeSpan colorWord hex = TL.concat [" ", t, " "] where
  t = renderText $ span_ attrs (toHtml colorWord)
  attrs = [ class_ "color", style_ (T.concat ["color: ", hex])::Attribute ]

-- | Utility for making printable data sets of the color name locations
-- so that they can be used in analysis later.
getZipData :: (Span, ColorOrNot) -> Maybe (Span, ColorFound, ColorStandardized)
getZipData (locs, parsed) = case parsed of
  Left _ -> Nothing
  Right (txtFormat, stdFormat) -> Just (locs, txtFormat, stdFormat)

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

-- | Utility to convert a list [("a", 2), ("a", 3), ("b", 2)] to a Map
-- like [("a", [2, 3]), "b", [2])]
listToMap :: [(Span, b, Text)] -> M.Map ColorWord [Span]
listToMap l = M.fromListWith (++) [(T.toLower stdFormat, [(loc1, loc2)]) |
                                   ((loc1, loc2), txtFormat, stdFormat) <- l]
