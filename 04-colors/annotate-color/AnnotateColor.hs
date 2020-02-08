{-# LANGUAGE OverloadedStrings #-}

module AnnotateColor where

import Data.List (intersperse, sort, sortBy)
import Lucid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import Data.Function (on)
import Replace.Attoparsec.Text
import Data.Attoparsec.Text as AT
import qualified Data.Text as T
import Data.Either
import Data.Char
import qualified Data.Map.Strict as M
import Control.Applicative ((<|>), empty)
import Options.Generic

-- | Just some useful aliases here
type ColorWord = T.Text
type Hex = T.Text

wordBoundary :: Parser Char
wordBoundary = satisfy (inClass " \n\r\"\'")

-- | Takes a list of words like "light green blue" and makes a
-- parser which will find "light green blue" and also "light green-blue",
-- "light green\nblue" and so on.
wordListParser :: [T.Text] -> Parser T.Text
wordListParser [w] = do -- One word case
  boundaryBefore <- wordBoundary
  word <- asciiCI w
  boundaryAfter <- wordBoundary
  return word
wordListParser (w:ws) = do  -- Multi-word case
  satisfy (inClass " \n\r\"\'")  -- Word boundary first
  a <- asciiCI w                  -- word, case insensitive
  b <- satisfy (inClass " -\n\r") -- a separator
  c <- wordListParser ws          -- more words
  return (a `T.append` (T.singleton b) `T.append` c) -- singleton :: Char -> Text

-- | Make one big parser out of our color map, and the expressions
-- generated from wordListParser.
colorParser :: [(ColorWord, Hex)] -> Parser T.Text
colorParser colormap = choice $ map (wordListParser . T.words . fst) $ colormap

-- | Makes HTML from a color word and hex pair.
-- I.e. "red" -> "<span class="color" style="color: #ff0000">"
makeSpan :: T.Text -> T.Text -> TL.Text
makeSpan colorWord hex = TL.concat [" ", t, " "] where
  t = renderText $ span_ attrs (toHtml colorWord)
  attrs = [ class_ "color", style_ (T.concat ["color: ", hex])::Attribute ]

-- | Maps a function across both items in a tuple
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)


-- | Processes the plain text (TSV) color map from XKCD,
-- and converts it to the list of tuples we'll be using for
-- a color map. 
xkcdMap :: T.Text -> [(T.Text, T.Text)]
xkcdMap rawMap = reverse $ sortBy (compare `on` T.length . fst) unsorted
  where
    textLines = tail $ T.lines rawMap
    unsorted = [ mapTuple T.strip ( T.breakOn "\t" ln ) | ln <- textLines ]

-- | The parser returns what it parsed, which may or may not contain
-- hyphens. But we want the non-hyphenated version so that we can
-- look up its hex in the color map. It sucks that we have to
-- look up the hex code again, but I can't think of a better way.
-- So this cleans up the color expression as found by the parser,
-- so that it can be used by the lookup.
punctToSpace :: T.Text -> T.Text
punctToSpace str = T.map p2s str where
  p2s = (\c -> if T.isInfixOf (T.singleton c) "-\n\r" then ' ' else c)

-- | Using a map-ified version of our color map, this looks
-- up each word found by the parser, and if found, turns it
-- into HTML, highlighting it using its color.
annotate :: M.Map ColorWord Hex -> T.Text -> T.Text
annotate cmm color = case cmm M.!? ((punctToSpace . T.strip) color) of
  Nothing -> (T.concat ["CANTFIND", color])
  Just hex -> TL.toStrict $ makeSpan (T.strip color) hex

-- | Adds the parser pattern to the word - hex tuple.
makeColorParser :: (ColorWord, Hex) -> (ColorWord, Hex, Parser T.Text)
makeColorParser (colorword, hex) = (colorword, hex, ((wordListParser . T.words) colorword))

findReplace :: (ColorWord, Hex, Parser T.Text) -> T.Text -> [Either T.Text T.Text]
findReplace (colorWord, hex, parser) sourceText = fromRight [] $ parseOnly (findAll parser) sourceText

-- | Only acts on the remaining unparsed text.
replaceRemaining :: (ColorWord, Hex, Parser T.Text) -> [Either T.Text T.Text] -> [Either T.Text T.Text]
replaceRemaining (colorWord, hex, parser) sourceText = concatMap parseBit sourceText where
  parseBit :: Either T.Text T.Text -> [Either T.Text T.Text]
  parseBit textBit = case textBit of
    Left t -> findReplace (colorWord, hex, parser) t -- Unparsed, parse it
    Right t -> [Right t] -- Parsed, pass through


main :: IO ()
main = do
    -- Load color map. (Data from https://xkcd.com/color/rgb.txt )
    rawText <- TIO.readFile "../data/xkcd/rgb.txt"
    -- Process color map
    let colorMap = xkcdMap rawText
    -- Make Data.Map map out of it
    -- let cmm = M.fromList cm

    -- Add parsers to the map
    let colorMapWithParsers = map makeColorParser colorMap

    -- Parse command-line argument, and read the filename given
    -- by the first argument.
    fileName <- getRecord "Color word annotator."
    inFile <- TIO.readFile fileName
    -- Run the parser, annotate it, print the results.
    -- TIO.putStr $ streamEdit (colorParser cm) (annotate cmm) inFile

    let firstReplaced = findReplace (head colorMapWithParsers) inFile
    let allReplaced = foldl (map (\mapping -> replaceRemaining mapping) colorMapWithParsers) firstReplaced

    print allReplaced
