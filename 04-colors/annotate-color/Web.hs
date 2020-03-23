{-# LANGUAGE OverloadedStrings #-}

module Web where

import           Control.Monad                        (join, forM_)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Applicative                  ((<$>))
import           Data.Maybe                           (fromMaybe)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           System.Environment                   (lookupEnv)
import           Text.Read                            (readMaybe)
import           Web.Scotty 


import Network.Wai.Parse
import System.FilePath ((</>))

import qualified Clay as C
import Lucid

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS

import Main

import ColorMaps

homepage :: Html ()
homepage = do
  html_ $ do
    head_ [] $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      style_ [type_ "text/css"] $ C.render css
      -- plotlyCDN
      link_ [ rel_ "stylesheet",
              href_ "https://unpkg.com/spectre.css/dist/spectre.min.css" ]
      link_ [ rel_ "stylesheet",
              href_ "https://unpkg.com/spectre.css/dist/spectre-icons.min.css" ]
    body_ $ do
      main_ [ class_ "container" ] $ do
        div_ [ class_ "hero bg-gray" ] $ do
          div_ [ class_ "hero-body" ] $ do
            h1_ "Color Word Analyzer"
            p_ "This tool searches your text for color words, or passages with colorful content."
        uploadForm
      scripts

uploadForm :: Html ()
uploadForm = do
  form_ [method_ "post", enctype_ "multipart/form-data", action_ "/upload" ] $ do
    div_ [ class_ "form-group" ] $ do
      label_ [for_ "colorMapSelector"] "Choose a word-to-color mapping."
      div_ [ class_ "form-group" ] $ do
        select_ [id_ "colorMapSelector", name_ "colorMap", class_ "form-select" ] $
          forM_ colorMaps (\colorMap -> option_ [value_ (name colorMap)]
                            (toHtml (name colorMap)))
      label_ [ for_ "fileSelector" ] "Choose a file to upload."
      input_ [ class_ "form-input", type_ "file",
                name_ "uploadedFile", id_ "fileSelector" ]
      input_ [ class_ "btn btn-primary", type_ "submit" ]

navBar :: Html ()
navBar = header_ [ class_ "navbar" ] $ do
  section_ [ class_ "navbar-section" ] $ do
    "Jonathan Reeve"
  section_ [ class_ "navbar-section" ] $ do
    a_ [ class_ "btn btn-link" ] $ toHtml "upload"
    a_ [ class_ "btn btn-link" ] $ toHtml "about"

scripts :: Html ()
scripts = mapM_ (\src -> with (script_ "") [ src_ src ]) [ "" ]

-- | The styling for the result web page
css :: C.Css
css = do
  "main" C.? do
    C.maxWidth (C.pct 80)
  "div.annotated" C.? do
    C.backgroundColor "#555"
    C.color "#ddd"

main :: IO ()
main = do
  port <- fromMaybe 3000
    . join
    . fmap readMaybe <$> lookupEnv "PORT"
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "uploads/") -- for favicon.ico
    middleware logStdoutDev
    -- get "/" $ do
    --   beam <- param "word"
    --   html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
    -- get "/:word" $ do
    --   beam <- param "word"
    --   html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

    get "/" $ do
      html $ renderText $ homepage

    post "/upload" $ do
      cm <- param ("colorMap" :: TL.Text) :: ActionM TL.Text
      fs <- files
      let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
      -- liftIO $ print fs'
      -- write the files to disk, so they will be served by the static middleware
      liftIO $ sequence_ [ B.writeFile ("uploads" </> fn) fc | (_,fn,fc) <- fs' ]
      -- generate list of links to the files just uploaded
      html $ renderText $ analyze fc

analysisOutput :: Html ()
analysisOutput = 

analyze :: TL.Text -> Html ()
analyze fileContents = 
