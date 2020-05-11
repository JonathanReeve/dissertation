{-# LANGUAGE OverloadedStrings #-}

module Template where

-- | Template for dissertation HTML

import Lucid as L
import Clay as C
import Data.Text.Lazy as T
import Data.Text.Lazy.IO as TIO

pageHtml :: Html ()
pageHtml = do
  html_ $ do
    head_ $ do
      meta_ [ charset_ "utf-8" ]
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0, user-scalable=yes" ]
      meta_ [ name_ "author", content_ "Jonathan Reeve" ]
      meta_ [ name_ "dcterms.date", content_ "$date-meta$" ]
      title_ "$if(title-prefix)$$title-prefix$ – $endif$$pagetitle$"
      style_ [ L.type_ "text/css" ] $ C.render css
      -- Print styling argh
      style_ [ L.type_ "text/css" ] ("@page { margin: 3cm; @bottom-center { content: counter(page); } }" :: Html ())
      link_ [ rel_ "stylesheet", href_ "../templates/tufte-css/tufte.css" ]
      link_ [ rel_ "stylesheet", href_ "../templates/tufte-css/latex.css" ]
    body_ $ do
      "\n $for(include-before)$ \n $include-before$ \n $endfor$ \n"
      header_ $ do
        h1_ [ class_ "title" ] "$title$"
        -- p_ [ class_ "subtitle" ] "$subtitle$"
        p_ [ class_ "author" ] "$author$"
        p_ [ class_ "date" ] "$date$"
      L.main_ $ do
        "$if(toc)$ \n"
        nav_ [ id_ "$idprefix$TOC", role_ "doc-toc" ] "$table-of-contents$ \n"
        "$endif$ \n $body$ \n $for(include-after)$ \n $include-after$ \n $endfor$ \n"

css :: Css
css = do
  p ? do
    lineHeight (unitless 2)
    color red


marginAll m = margin m m m m
