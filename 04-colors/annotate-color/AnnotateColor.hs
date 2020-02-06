{-# LANGUAGE OverloadedStrings #-}

module AnnotateColor where
  
import Data.List (intersperse, sort, sortBy)
import Lucid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Function (on)
import Replace.Attoparsec.Text
import Data.Attoparsec.Text as AT
import qualified Data.Text as T
import Data.Either
import Data.Char
import qualified Data.Map.Strict as M
import Control.Applicative ((<|>))

s :: T.Text
s = "I have several paints. For example, red, green, and blue, and blue-green."

-- | Words can be separated by spaces, hyphens, or line breaks
-- Got some help from https://stackoverflow.com/q/60085733/584121
legalSep :: Parser T.Text
legalSep = T.pack . pure <$> satisfy (inClass "- \n\r")

variations :: T.Text -> Parser [T.Text]
variations = sequence . intersperse (mempty <$ many1 legalSep) . 
                   fmap string . T.words

colorParser :: [(ColorWord, Hex)] -> Parser [T.Text]
colorParser colormap = choice $ map (variations . fst) $ colormap

-- | Makes HTML from a color word and hex pair.
makeSpan :: T.Text -> T.Text -> Html ()
makeSpan colorWord hex = span_ attrs (toHtml colorWord) where
  attrs = [ class_ "color", style_ (T.concat ["color: ", hex])::Attribute ]

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

-- data ColorMap = ColorMap [(ColorWord, Hex)] deriving Show
-- type ColorMap = [(ColorWord, Hex)]
type ColorWord = T.Text
type Hex = T.Text

colorWord :: (ColorWord, Hex) -> ColorWord
colorWord (cw, hex) = cw

xkcdMap :: T.Text -> [(ColorWord, Hex)]
xkcdMap rawMap = reverse $ sortBy (compare `on` T.length . fst) unsorted
  where
    textLines = tail $ T.lines rawMap
    unsorted = [ mapTuple T.strip ( T.breakOn "\t" ln ) | ln <- textLines ]

lookupColor :: T.Text -> [(ColorWord, Hex)] -> Maybe Hex
lookupColor colorExpr cm = M.lookup colorExpr (M.fromAscList cm)

annotate :: [(ColorWord, Hex)] -> Either T.Text T.Text -> Html ()
annotate cm item = case item of
  Left expr -> toHtml expr -- Not a color expression. Pass through.
  Right expr -> case lookupColor (expr::ColorWord) cm of  -- Look up the word. FIXME
    Nothing -> toHtml expr
    Just hex -> makeSpan hex expr

main :: IO ()
main = do
    rawText <- TIO.readFile "../data/xkcd/rgb.txt"
    let cm = xkcdMap rawText
    let parsed = fromRight [] $ parseOnly (findAll (colorParser cm)) s
    print $ map (annotate cm) parsed


