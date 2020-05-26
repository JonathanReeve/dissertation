{-# LANGUAGE OverloadedStrings #-}
-- | Template for dissertaiton HTML

import Lucid as L
import Clay as C
import Data.Text.Lazy as T
import Data.Text.Lazy.IO as TIO

main :: IO ()
main = TIO.putStr $ renderText $ pageHtml

pageHtml = do
  html_ $ do
    head_ $ do
      meta_ [ charset_ "utf-8" ]
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0, user-scalable=yes" ]
      meta_ [ name_ "author", content_ "Jonathan Reeve" ]
      meta_ [ name_ "dcterms.date", content_ "$date-meta$" ]
      title_ "$if(title-prefix)$$title-prefix$ – $endif$$pagetitle$"
      style_ [ L.type_ "text/css" ] $ C.render css
    body_ $ do
      "$for(include-before)$ \n $include-before$ \n $endfor$ \n $if(title)$"
      header_ $ do
        h1_ [ class_ "title" ] "$title$"
        -- p_ [ class_ "subtitle" ] "$subtitle$"
        p_ [ class_ "author" ] "$author$"
        p_ [ class_ "date" ] "$date$"
      L.main_ $ do
        "$if(toc)$"
        nav_ [ id_ "$idprefix$TOC", role_ "doc-toc" ] "$table-of-contents$"
        "$endif$"
        "$body$"
        "$for(include-after)$ \n $include-after$ \n $endfor$"

css :: Css
css = p ? color red
