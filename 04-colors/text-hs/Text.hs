{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc
import Text.Pandoc.Walk (walk)
import TEI

import Text.Parsec
import Text.Parsec.Text (Parser)
import Text.Numeral.Roman
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

transform :: Pandoc -> Pandoc
transform doc = markChapter doc -- markChapter doc

-- -- Example from here: https://pandoc.org/using-the-pandoc-api.html#walking-the-ast
markChapter :: Pandoc -> Pandoc
markChapter = walk mark
  where mark :: Block -> Block
        mark (Header lev attr inlines) =
          if (isChapter (head inlines))
          then
            Header lev (stainAttr attr) inlines
          else
            Header lev attr inlines
        mark x = x
        stainAttr (htmlClass, secondThing, attrDict) = (htmlClass, secondThing, [("chapter", "true")])

arabicNum :: Parser Int
arabicNum = do
  n <- many1 digit
  return (read n)

isChapter :: Inline -> Bool
isChapter str = str == Str "CHAPTER"

chapterWords :: Parser String
chapterWords = (string "Chapter") <|> (string "CHAPTER")

number :: Parser Int
number = arabicNum <|> romanNum

romanNum :: Parser Int
romanNum = do
  str <- many1 anyChar
  case fromRoman str of
    Just n -> return n
    Nothing -> fail $ str ++ " is not a valid roman numeral"

regularParse :: Parser a -> T.Text -> Either ParseError a
regularParse p = parse p ""


main :: IO ()
main = do
  -- Read stdin
  input <- TIO.getContents
  result <- runIO $ do
    doc <- readHtml def input
    TEI.writeTEI def (transform doc)
  rst <- handleError result
  -- Write stdout
  TIO.putStrLn rst
