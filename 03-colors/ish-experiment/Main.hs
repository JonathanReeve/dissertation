{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List
import Data.List.Split
import Lucid
import qualified Data.Text.Lazy.IO as TIO
import Data.Containers.ListUtils (nubOrd)

main :: IO ()
main = do
  rawText <- readFile "../data/maps/xkcd/rgb.txt"
  let colorNames = concatMap (take 1 . splitOn "\t") (lines rawText)
  let ishes = filter (isInfixOf "ish") colorNames
  let rows = groupSortBy (take 1 . words) ishes
  let cols = groupSortBy (last . words) ishes
  let lastWords = nubOrd $ map (last . words) ishes
  print rows
  print lastWords
  -- print
  -- print $ length cols
  -- TIO.putStr $ renderText $ html_ $ do
  --   body_ $ do
  --     table_ [] $
        -- mapM (th_ []) lastWords
  -- print $ mapM (categorizeRow lastWords) rows

-- | Given a row of words, categorize them according to their last
-- words, and return tuples of (colorWord, category).
-- Or, for each item in the category, say whether it has a word in
-- our wordlist.
categorizeRow :: [String] -> [String] -> [(String, String)]
categorizeRow colNames row = map makeTuple colNames where
  makeTuple colname = (colname, colorMaybe) where
    lastWordIs a b = (last . words) a == (last . words) b
    candidates = filter (lastWordIs colname) row
    colorMaybe = if candidates == []
      then "NA"
      else head candidates

-- formatRow :: [String] -> [String] -> Html [()]
-- formatRow colNames row = tr_ [] $ mapM formatItem intersection where
--   intersection = intersectBy (\a b -> lastWord a == lastWord b) row colNames
--   lastWord = last $ words item
--   formatItem :: String -> Html ()
--   formatItem item = td_ [] maybeItem where
--     maybeItem = if lastWord `elem` colNames
--       then toHtml item
--       else "NA" :: Html ()

-- groupSortBy :: (String -> String) -> [String] -> [[String]]
groupSortBy pred xs = groupBy (\a b -> pred a == pred b) $ sortOn pred xs
