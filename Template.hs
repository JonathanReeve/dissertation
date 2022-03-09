{-# LANGUAGE OverloadedStrings #-}

module Template where

-- Template for dissertation HTML

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
      link_ [ rel_ "stylesheet", href_ "/assets/tufte-css/latex.css" ]
      link_ [ rel_ "stylesheet", href_ "/assets/tufte-css/tufte.css" ]
      script_ [ src_ "/04-colors/includes/plotly-latest.min.js" ] T.empty
      style_ [ L.type_ "text/css" ] ("@page { margin: 3cm; @bottom-center { content: counter(page); } }" :: Html ())
    body_ $ do
      article_ $ do
        "\n $for(include-before)$ \n $include-before$ \n $endfor$ \n"
        header_ $ do
          h1_ [ class_ "title" ] "$title$"
          -- p_ [ class_ "subtitle" ] "$subtitle$"
          p_ [ class_ "subtitle author" ] "$author$"
          p_ [ class_ "date" ] "$date$"
        L.main_ $ do
          "$if(toc)$ \n"
          details_ $ do
            summary_ "Table of Contents"
            nav_ [ id_ "$idprefix$TOC", role_ "doc-toc" ] "$table-of-contents$ \n"
            "$endif$ \n"
          "$body$ \n $for(include-after)$ \n $include-after$ \n $endfor$ \n"
      footer_ $ do
        script_ [ src_ "https://polyfill.io/v3/polyfill.min.js?features=es6" ] T.empty
        script_ [ id_ "MathJax-script" , async_ ""
                , src_ "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" ] T.empty
        script_ [src_ "https://hypothes.is/embed.js"] T.empty
        -- Flowcharts
        script_ [ src_ "includes/mermaid.min.js" ] T.empty
        -- Required for jquery-lazy
        script_ [ src_ "includes/zepto.min.js" ] T.empty
        -- Try to lazy-load stuff
        script_ [ src_ "includes/jquery.lazy.min.js" ] T.empty
        script_ [ src_ "includes/custom.js" ] T.empty


css :: Css
css = do
  p ? do
    lineHeight (unitless 2)
  ".colorBlock" ? paddingAll (em 0.2)
  td ? sym padding (em 0.3)
  "div.box" ? do
    backgroundColor "#cbcbf7"
    sym borderRadius (px 10)
    sym padding (em 0.8)
  "div.references" ? do
    lineHeight (unitless 2)
    marginLeft (em 1)
    textIndent (indent (em (-1)))
    fontSize (C.rem 1.4)

marginAll m = margin m m m m
paddingAll p = padding p p p p
