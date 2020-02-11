{-# LANGUAGE OverloadedStrings #-}

module AnnotateColor where

import qualified Clay as C
import Codec.Text.Detect (detectEncoding)
import Control.Applicative ((<|>), empty)
import Control.Monad (forM_)
import Data.Attoparsec.Text as AT
import Data.Char
import Data.Either
import Data.Function (on)
import Data.List (intersperse, sort, sortBy)
import Lucid
import Options.Generic
import Replace.Attoparsec.Text
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

-- | Just some useful aliases here
type ColorWord = T.Text
type Hex = T.Text

wordBoundary :: Parser Char
wordBoundary = space <|> satisfy isPunctuation

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


-- | Processes the plain text (TSV) color map from XKCD,
-- and converts it to the list of tuples we'll be using for
-- a color map. 
xkcdMap :: T.Text -> [(T.Text, T.Text)]
xkcdMap rawMap = reverse $ sortBy (compare `on` T.length . fst) unsorted
  where
    textLines = tail $ T.lines rawMap
    unsorted = [ mapTuple T.strip ( T.breakOn "\t" ln ) | ln <- textLines ]

annotate :: M.Map ColorWord Hex -> [Either T.Text (T.Text, T.Text)] -> T.Text
annotate colorMapMap results = T.concat $ map processBlock results where
  processBlock :: Either T.Text (T.Text, T.Text) -> T.Text
  processBlock block = case block of
    Left txt -> txt
    Right (textFormat, stdFormat) -> case (colorMapMap M.!? stdFormat) of
      Nothing -> T.concat ["CANTFIND", stdFormat]
      Just hex -> TL.toStrict $ makeSpan textFormat hex

findReplace :: Parser T.Text -> T.Text -> [Either T.Text (T.Text, T.Text)]
findReplace parser sourceText = fromRight [] $ parseOnly (findAllCap parser) sourceText

-- | Make a standard HTML page from the results,
-- by scaffolding it with an HTML template.
scaffold :: T.Text -> T.Text
scaffold text = TL.toStrict $ renderText $
  html_ [] $ do
    head_ [] $ do
      style_ [type_ "text/css"] $ C.render css
    body_ [] $ toHtmlRaw text

css :: C.Css
css = "body" C.? do
         C.backgroundColor "#333"
         C.color "#ddd"

main :: IO ()
main = do
   -- Load color map. (Data from https://xkcd.com/color/rgb.txt )
   rawText <- TIO.readFile "../data/xkcd/rgb.txt"
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

   let firstReplaced = findReplace (colorParser colorMap) inFile

   -- print firstReplaced
   let annotated = annotate colorMapMap firstReplaced
   let scaffolded = scaffold annotated

   TIO.putStr $ scaffolded
