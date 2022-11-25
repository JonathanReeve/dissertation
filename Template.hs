{-# LANGUAGE OverloadedStrings #-}

module Template where

-- Template for dissertation HTML

import Lucid as L
import Clay as C
import Data.Text.Lazy as T
import Data.Text.Lazy.IO as TIO

prefatoryPageHtml :: Html ()
prefatoryPageHtml = do
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
    body_ [ class_ "prefatory" ] $ do
      article_ $ do
        "\n $for(include-before)$ \n $include-before$ \n $endfor$ \n"
        header_ [ id_ "prefatory" ] $ do
          -- Title "Page" Section
          section_ [ id_ "titlePage" ] $ do
            h1_ [ class_ "title" ] "$title$"
            p_ [ class_ "subtitle" ] "$subtitle$"
            p_ [ class_ "subtitle author" ] "$author$"
            p_ [ class_ "purpose" ] "Submitted in partial fulfillment of the requirements for the degree of Doctor of Philosophy under the Executive Committee of the Graduate School of Arts and Sciences"
            p_ [ class_ "date" ] "2023"
          section_ [ id_ "copyrightPage" ] $ do
            p_ [ class_ "copyright" ] "© 2023 Jonathan Reeve. All Rights Reserved."
            p_ $ do
              "All text released under the "
              a_ [ href_ "https://creativecommons.org/licenses/by-nc/4.0/" ] "Creative Commons Attribution-NonCommercial 4.0 International License (CC BY-NC 4.0)"
              ". All source code released under the "
              a_ [ href_ "https://www.gnu.org/licenses/gpl-3.0.html" ] "GNU General Public License, Version 3"
              ". Please find the canonical edition of this work at "
              a_ [ href_ "https://dissertation.jonreeve.com" ] "dissertation.jonreeve.com, "
              "and its source code at "
              a_ [ href_ "https://github.com/JonathanReeve/dissertation" ] "github.com/JonathanReeve/dissertation."
        L.main_ $ do
          "$body$ \n $for(include-after)$ \n $include-after$ \n $endfor$ \n"
      footer_ $ do
        script_ [ src_ "https://polyfill.io/v3/polyfill.min.js?features=es6" ] T.empty
        script_ [ id_ "MathJax-script" , async_ ""
                , src_ "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" ] T.empty
        script_ [src_ "https://hypothes.is/embed.js"] T.empty
        script_ [ src_ "includes/custom.js" ] T.empty

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
    important $ lineHeight (unitless 2)
  ".colorBlock" ? sym padding (em 0.2)
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
  -- Prefatory "pages"
  "body.prefatory" ? do
    paddingLeft (unitless 0)
    "main" ? paddingLeft (pct 12.5)
    "header#prefatory" ? do
      textAlign C.center
      marginLeft (pct 12.5)
      marginRight (pct 12.5)
      -- Disable width = 55% from Tufte
      "section > p" ? width (pct 100)
    "section#titlePage, section#copyrightPage" ? do
      marginTop (em 5)
      marginBottom (em 5)
    "p.purpose" ? marginTop (em 5)
