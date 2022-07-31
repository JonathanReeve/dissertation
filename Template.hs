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
      script_ [ src_ "/03-colors/includes/plotly-latest.min.js" ] T.empty
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
  figure ? do
    textAlign C.center
  -- Automatically enlarge images on hover
  figure |> img # hover ? transform (scale 1.5 1.5)
  figure |> figcaption ? do
    maxWidth none
    textAlign $ alignSide sideLeft
  figure |> img ? do
    maxHeight (em 30)
  -- Info boxes, like the one at the top of the page.
  "div.box" ? do
    sym margin (em 1)
    backgroundColor "#cbcbf7"
    sym borderRadius (px 10)
    sym padding (em 0.8)
    width (pct 50)
    borderBottom (px 1) solid "#999"
    borderLeft (px 1) solid "#999"
    -- Fix weird-looking white background in links in boxes
    "a" ? do
      textShadow (px 0) (px 0) (px 0) "#000"
  "div.line-block" ? do
    marginLeft (em 2)
    fontSize (C.rem 1.4)
  -- Color-annotated block quotes.
  "div.annotated" ? do
    -- We make this gray so that white text can appear on it.
    backgroundColor "#ccc"
    sym borderRadius (px 10)
    sym padding (em 0.8)
    fontSize (C.rem 1.4)
    maxWidth (em 25)
    marginLeft (em 2)
    borderBottom (px 1) solid "#999"
    borderLeft (px 1) solid "#999"
    "a" ? do
      textShadow (px 0) (px 0) (px 0) "#000"
  "div.references" ? do
    lineHeight (unitless 2)
    marginLeft (em 1)
    textIndent (indent (em (-1)))
    fontSize (C.rem 1.4)
  details ? do
    fontSize (C.rem 1.5)
    margin (em 1) (em 0) (em 1) (em 0)


marginAll m = margin m m m m
paddingAll p = padding p p p p
