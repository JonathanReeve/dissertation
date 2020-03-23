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

import Data.Maybe

import Network.Wai.Parse
import System.FilePath ((</>), takeBaseName)

import qualified Clay as C
import qualified Data.Map as M
import Lucid

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS

import Graphics.Plotly.Lucid (plotlyCDN)

-- import Main

import qualified ColorMaps as CM
import FindColors
import AnnotateColors
import PlotColors
import Types

scaffold :: Html () -> Html ()
scaffold contents = do
  html_ $ do
    head_ [] $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      style_ [type_ "text/css"] $ C.render css
      plotlyCDN
      link_ [ rel_ "stylesheet",
              href_ "https://unpkg.com/spectre.css/dist/spectre.min.css" ]
      link_ [ rel_ "stylesheet",
              href_ "https://unpkg.com/spectre.css/dist/spectre-icons.min.css" ]
    body_ $ do
      navBar
      main_ [ class_ "container" ] $ do
        contents
      scripts


homepage :: Html ()
homepage = scaffold $ do
  div_ [ class_ "hero bg-gray" ] $ do
    div_ [ class_ "hero-body" ] $ do
      h1_ "Color Word Analyzer"
      p_ "This tool searches your English-language text for color words, or passages with colorful content."
  uploadForm

uploadForm :: Html ()
uploadForm = do
  form_ [method_ "post", enctype_ "multipart/form-data", action_ "/upload" ] $ do
    div_ [ class_ "form-group" ] $ do
      span_ [ style_ "large" ] "1"
      label_ [for_ "colorMapSelector"] "Choose a word-to-color mapping."
      div_ [ class_ "form-group" ] $ do
        select_ [id_ "colorMapSelector", name_ "colorMap", class_ "form-select" ] $
          forM_ CM.colorMaps (\colorMap -> option_ [value_ (CM.name colorMap)]
                            (toHtml (CM.name colorMap)))
      label_ [ for_ "fileSelector" ] "Choose a file to upload."
      input_ [ class_ "form-input", type_ "file",
                name_ "uploadedFile", id_ "fileSelector" ]
      input_ [ class_ "btn btn-primary", type_ "submit" ]

navBar :: Html ()
navBar = header_ [ class_ "navbar container bg-primary text-secondary" ] $ do
  section_ [ class_ "navbar-section" ] $ do
    "Color Word Imaginer"
  section_ [ class_ "navbar-section" ] $ do
    a_ [ class_ "btn btn-link text-secondary" ] $ ("about" :: Html ())
    a_ [ class_ "btn btn-link text-secondary" ] $ ("upload" :: Html ())

scripts :: Html ()
scripts = mapM_ (\src -> with (script_ "") [ src_ src ]) [ "" ]

-- | The styling for the result web page
css :: C.Css
css = do
  "main, header" C.? do
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

    get "/" $ do
      html $ renderText $ homepage

    post "/upload" $ do
      cm <- param ("colorMap" :: TL.Text) :: ActionM TL.Text
      fs <- files
      let fs' = head [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
      -- liftIO $ print fs'
      -- write the files to disk, so they will be served by the static middleware
      let (_, fn, fc) = fs'
      liftIO $ B.writeFile ("uploads" </> fn) fc
      -- generate list of links to the files just uploaded
      colorMap <- liftIO $ CM.assoc $ CM.getColorMap cm
      let contents = readInfile $ B.toStrict fc
      let label = takeBaseName fn
      html $ renderText $ doAnalysis contents label colorMap (CM.name (CM.getColorMap cm))

readInfile :: BS.ByteString -> T.Text
readInfile fc = case TE.decodeUtf8' fc of
               Left err -> TE.decodeLatin1 fc
               Right text -> text

doAnalysis :: T.Text -> -- | Input file
             String -> -- | Input file label
             [(Types.ColorWord, Types.Hex)] -> -- | Color mapping
             T.Text -> -- | Color mapping label
             Html () -- | Resulting HTML
doAnalysis inFile label colorMap colorMapLabel = do
  -- let colorMap' = CM.extendMap colorMap
  let colorMapMap = M.fromList colorMap

  let parsed = findReplace (colorParser colorMap) inFile
  let zipData = map getZipData (zip (getLocations parsed) parsed)
  -- let onlyMatches = map fromJust $ filter isJust zipData
  let onlyMatches = catMaybes zipData
  let stats = makeStats (T.pack label) colorMapLabel (listToMap onlyMatches) colorMapMap
  mkHtml colorMapMap [stats] parsed (T.length inFile)

mkHtml :: Types.ColorMap -> ColorStatsMap -> [ColorOrNot] -> Int -> Html ()
mkHtml colorMap stats parsed len = scaffold $ do
    h1_ [] "Color Words in Aggregate"
    let barTraces = (mkHBarTraces stats) ++ (mkHBarParentTraces colorMap stats)
    plotlyChart' barTraces "div1"
    h1_ [] "Color Words in Narrative Timeseries"
    let lineTraces = mkChunkedTraces stats len 10
    plotlyChart' lineTraces "div2"
    h1_ [] "Annotated Text"
    let annotated = annotate colorMap parsed
    div_ [ class_ "annotated" ] $ toHtmlRaw annotated
