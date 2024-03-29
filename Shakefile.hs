-- Shakefile Stuff
import Development.Shake
import Development.Shake.FilePath
import Text.Regex
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import Data.Text.ICU (regex, Regex)
-- import Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified Data.Text.ICU.Replace as TR

-- Server stuff
import Network.Wai.Application.Static (defaultFileServerSettings, ssListing, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import WaiAppStatic.Types (StaticSettings)

import Main.Utf8 (withUtf8)
import Lucid
import Template ( pageHtml, prefatoryPageHtml, includeHtml )

import Text.HTML.TagSoup

import Text.StringLike ( StringLike )
import Data.Maybe (fromMaybe)

readFileText text = need [text] >> liftIO (TIO.readFile text)

-- | Convert "00-introduction/introduction-v01.org" to "dest/00-introduction/introduction-v01.html"
sourceToDest :: FilePath -> FilePath
sourceToDest fp = "dest/" </> fp <.> "html"

-- | Convert "00-introduction/introduction-v01.org" to "dest/00-introduction/introduction-v01.html"
destToSource :: FilePath -> FilePath
destToSource fp = dropDirectory1 $ fp -<.> "org"

chapters :: [FilePath]
chapters =
  [ "dest/00-introduction/introduction-v01.html",
    "dest/01-colors/ch-1-v01.html",
    "dest/02-shapes/ch-2-v01.html",
    "dest/03-images/ch-3-v01.html"
  ]

main :: IO ()
main = withUtf8 $ shakeArgs shakeOptions{shakeColor=True} $ do
    want ( "dest/index.html" : chapters )

    -- To serve the generated files (useful for previewing),
    -- run `shake serve`.
    phony "serve" $
      liftIO $ serve 8080 "dest/"

    -- Regenerate references bibtex file, but only if we're me, not GitHub Actions.
    "references.bib" %> \f -> do
        let sources = [ "/home/jon/Dokumentujo/Papers/library.bib"
                      , "/home/jon/Dokumentujo/Papers/library2.bib"
                      ]
        user <- getEnv "USERNAME"
        let username = fromMaybe "" user
        if username == "jon" then do
            need sources
            Stdout stdout <- cmd "cat" sources
            writeFileChanged f stdout
        else putInfo "We're in CI. Skipping regeneration of references."

    -- Automatically generate a list of figures and illustrations.
    "templates/figures.html" %> \f -> do
        need chapters
        liftIO $ findAllFigures f

    let bib = "references.bib"
        csl = "templates/modern-language-association.csl"
        template = "templates/template.html"
        prefatoryTemplate = "templates/prefatoryTemplate.html"
        figuresList = "templates/figures.html"

    "templates/template.html" %> \f -> do
        need ["Template.hs"]
        liftIO $ renderToFile f pageHtml

    "templates/prefatoryTemplate.html" %> \f -> do
        need ["Template.hs"]
        liftIO $ renderToFile f prefatoryPageHtml

    "dest/index.html" %> \f -> do
        let source = destToSource f
        need [ source, template, prefatoryTemplate, figuresList ]
        contents <- liftIO $ readFile source
        cmd (Stdin contents) "pandoc" ["-f", "org+smart",
                                       "--template", prefatoryTemplate,
                                       "--standalone",
                                       "--section-divs",
                                       "--variable=autoSectionLabels:true",
                                       "-o", f
                                       ]


    ["dest//images/*", "dest//includes/*.js", "dest/assets/**"] |%> \f -> do
        let source = dropDirectory1 f
        need [source]
        copyFileChanged source f

    "dest//includes/*.html" %> \f -> do
        -- wrap exported includes in HTML so they can be displayed on their own
        let source = dropDirectory1 f
        need [source]
        contents <- readFileText source
        let wrapped = includeHtml contents
        writeFileChanged f $ T.unpack $ LT.toStrict $ Lucid.renderText wrapped


    "dest/00-introduction/introduction-v01.html" %> \f -> do
        assets <- getDirectoryFiles "" [ "00-introduction/images/*"
                                       , "assets//*"
                                       ]
        let outAssets = map ("dest/" <>) assets
        let source = destToSource f
        need ([ source, prefatoryTemplate, csl, bib ] ++ outAssets)
        contents <- readFileText source
        let replaced = T.unpack contents
        cmd (Stdin replaced) "pandoc" ["-f", "org+smart",
                                       "--template", template,
                                       "--standalone",
                                       "--section-divs",
                                       "--reference-location=block",
                                       "--csl=" ++ csl,
                                       "--variable=autoSectionLabels:true",
                                       "--toc",
                                       "--metadata=tblPrefix:table",
                                       "--metadata=linkReferences:true",
                                       "--metadata=link-citations:true",
                                       "--filter=templates/PandocSidenote.hs",
                                       "--filter=pandoc-crossref",
                                       "--citeproc",
                                       "--mathjax",
                                       "--bibliography", bib,
                                       "-o", f
                                       ]

    "dest/01-colors/ch-1-v01.html" %> \f -> do
        assets <- getDirectoryFiles "" [ "01-colors/images/*"
                                       , "01-colors/includes/*"
                                       , "assets/*/*" -- Global assets
                                       ]
        liftIO $ print assets
        let source = destToSource f
        let outAssets = map ("dest/" <>) assets
            filters = [ "templates/PandocSidenote.hs"
                      , "templates/hexFilter.hs"
                      ]
        need ([ source, template, csl, bib ]
              ++ outAssets
              ++ filters)
        contents <- readFileText source
        let replaced = T.unpack contents
        cmd (Stdin replaced) "pandoc" ["-f", "org+smart",
                                       "--template", template,
                                       "--standalone",
                                       "--section-divs",
                                       "--reference-location=block",
                                       "--csl=" ++ csl,
                                       "--toc",
                                       "--variable=autoSectionLabels:true",
                                       "--metadata=linkReferences:true",
                                       "--metadata=link-citations:true",
                                       "--metadata=tblPrefix:table",
                                       "--citation-abbreviations=01-colors/abbreviations.json",
                                       "--filter=templates/PandocSidenote.hs",
                                       "--filter=pandoc-crossref",
                                       "--citeproc",
                                       "--filter=templates/hexFilter.hs",
                                       "--mathjax",
                                       "--bibliography", bib,
                                       "-o", f
                                       ]

    "dest/02-shapes/ch-2-v01.html" %> \f -> do
        assets <- getDirectoryFiles "" [ "02-shapes/images/*"
                                       , "02-shapes/includes/*"
                                       , "assets/*/*"
                                       ]
        liftIO $ print assets
        let outAssets = map ("dest/" <>) assets
        let source = destToSource f
            filters = [ "templates/PandocSidenote.hs"
                      , "templates/synsetFilter.hs"
                      ]
        need ([ source, template, csl, bib ]
              ++ outAssets
              ++ filters)
        contents <- readFileText source
        let replaced = T.unpack contents
        cmd (Stdin replaced) "pandoc" ["-f", "org+smart",
                                       "--template", template,
                                       "--standalone",
                                       "--section-divs",
                                       "--reference-location=block",
                                       "--csl=" ++ csl,
                                       "--toc",
                                       "--variable=autoSectionLabels:true",
                                       "--metadata=linkReferences:true",
                                       "--metadata=link-citations:true",
                                       "--metadata=tblPrefix:table",
                                       "--filter=templates/PandocSidenote.hs",
                                       "--filter=pandoc-crossref",
                                       "--citeproc",
                                       "--filter=templates/synsetFilter.hs",
                                       "--mathjax",
                                       "--bibliography", bib,
                                       "-o", f
                                       ]

    "dest/03-images/ch-3-v01.html" %> \f -> do
        assets <- getDirectoryFiles "" [ "03-images/images/*"
                                       , "assets/*/*"
                                       , "03-images/includes/*"
                                       ]
        let outAssets = map ("dest/" <>) assets
        liftIO $ print outAssets
        let source = destToSource f
            filters = [ "templates/PandocSidenote.hs"
                      , "templates/synsetFilter.hs"
                      ]
        need ([ source, template, csl, bib ]
              ++ outAssets
              ++ filters)
        contents <- readFileText source
        let replaced = T.unpack contents
        cmd (Stdin replaced) "pandoc" ["-f", "org+smart",
                                       "--template", template,
                                       "--standalone",
                                       "--section-divs",
                                       "--reference-location=block",
                                       "--csl=" ++ csl,
                                       "--toc",
                                       "--variable=autoSectionLabels:true",
                                       "--metadata=linkReferences:true",
                                       "--metadata=link-citations:true",
                                       "--metadata=tblPrefix:table",
                                       "--filter=templates/PandocSidenote.hs",
                                       "--filter=pandoc-crossref",
                                       "--citeproc",
                                       "--filter=templates/synsetFilter.hs",
                                       "--mathjax",
                                       "--bibliography", bib,
                                       "-o", f
                                       ]

-- | WAI Settings suited for serving statically generated websites.
staticSiteServerSettings :: FilePath -> StaticSettings
staticSiteServerSettings root =
  defaultSettings
    -- Disable directory listings
    { ssListing = Nothing }
  where
    defaultSettings = defaultFileServerSettings root

-- | Run a HTTP server to serve a directory of static files
serve ::
  -- | Port number to bind to
  Int ->
  -- | Directory to serve.
  FilePath ->
  IO ()
serve port path = do
  putStrLn $ "Serving at http://localhost:" <> show port
  Warp.run port $ staticApp $ staticSiteServerSettings path

-- | Automatically find figures and generate a list of chapters with their figures.
type HtmlString = String
type TagName = String
type FigureTags = [Tag String]

data ChapterFigures = ChapterFigures {
  chapterPath :: FilePath,
  chapterTitle :: String,
  figures :: [Figure]
  } deriving Show

data Figure = Figure {
  figCaption :: String,
  figId :: String
  } deriving Show

-- | Scrape HTML for some tag, but preserve the inner HTML
innerHtml :: TagName -> HtmlString -> [HtmlString]
innerHtml tagName rawText = let
  tag = "<" ++ tagName ++ ">"
  unTag = "</" ++ tagName ++ ">" in
  map (renderTags . takeWhile (~/= unTag) . tail) $
    sections (~== tag) $ parseTags rawText

getFigures :: HtmlString -> [FigureTags]
getFigures fileContents = map (takeWhile (~/= "</figure>")) $
  sections (~== "<figure>") $ parseTags fileContents

-- | Gets chapter names and figures for each chapter
-- Storing them in structured data 
getChapterFigures :: (FilePath, HtmlString) -> ChapterFigures
getChapterFigures (path, fileContents) = ChapterFigures path title figs where
  title = head $ innerHtml "title" fileContents
  figs = map figuresData $ getFigures fileContents

-- Takes parsed Html (Tags list) and returns structured data Figure
figuresData :: FigureTags -> Figure
figuresData figTags = Figure (figureCaption figTags) (figureId figTags) where
  figureId fig = fromAttrib "id" $ head $ filter (~== "<img>") fig
  figureCaption fig = renderTags $ takeWhile (~/= "</figcaption>") $ tail $ dropWhile (~/= "<figcaption>") fig

-- | Takes structured data about chapters and figures and returns HTML formatted text
formatChapterFigures :: ChapterFigures -> LT.Text
formatChapterFigures cf = Lucid.renderText $ ul_ $ title >> figs where
  title = li_ $ toHtml $ chapterTitle cf
  path = chapterPath cf
  figs = ul_ $ mconcat $ map formatFigure $ figures cf where
    formatFigure :: Figure -> Lucid.Html ()
    formatFigure fig = li_ $ a_ [href_ url] caption where
      caption = toHtmlRaw $ figCaption fig
      url = T.pack $
        drop 5 $ -- Remove "dest/"
        path ++ "#" ++ figId fig

-- | The main IO function for reading all the chapters and finding all the figures
findAllFigures :: FilePath -> IO ()
findAllFigures fn = do
  fileContents <- mapM readFile chapters
  let pathsAndContents = zip chapters fileContents
  let chapterFigures = map getChapterFigures pathsAndContents
  let chapterFiguresFormatted = map formatChapterFigures chapterFigures
  LTIO.writeFile fn $ LT.concat chapterFiguresFormatted
