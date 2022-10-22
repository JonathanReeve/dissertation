-- Shakefile Stuff
import Development.Shake
import Development.Shake.FilePath
import Text.Regex
import qualified Data.Text as T
import Data.Text.ICU (regex, Regex)
-- import Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified Data.Text.ICU.Replace as TR

-- Server stuff
import Network.Wai.Application.Static (defaultFileServerSettings, ssListing, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import WaiAppStatic.Types (StaticSettings)

import Main.Utf8 (withUtf8)
import Lucid (renderToFile)
import Template ( pageHtml )


readFileText text = need [text] >> liftIO (TIO.readFile text)

-- | Convert "00-introduction/introduction.org" to "dest/00-introduction/introduction.html"
sourceToDest :: FilePath -> FilePath
sourceToDest fp = "dest/" </> fp <.> "html"

-- | Convert "00-introduction/introduction.org" to "dest/00-introduction/introduction.html"
destToSource :: FilePath -> FilePath
destToSource fp = dropDirectory1 $ fp -<.> "org"

main :: IO ()
main = withUtf8 $ shakeArgs shakeOptions{shakeColor=True} $ do
    want [ "dest/index.html"
         , "dest/00-introduction/introduction.html"
         , "dest/01-colors/ch-1.html"
         , "dest/02-shapes/ch-2.html"
         , "dest/03-images/ch-3.html"
         ]

    -- To serve the generated files (useful for previewing),
    -- run `shake serve`.
    phony "serve" $
      liftIO $ serve 8080 "dest/"

    -- Keep this commented out in Git, since this references external files that aren't available in the CI
    -- "references.bib" %> \f -> do
    --     let sources = [ "/home/jon/Dokumentujo/Papers/library.bib"
    --                   , "/home/jon/Dokumentujo/Papers/library2.bib"
    --                   ]
    --     need sources
    --     Stdout stdout <- cmd "cat" sources
    --     writeFileChanged f stdout
    --     -- copyFileChanged source f

    "templates/template.html" %> \f -> do
        need ["Template.hs"]
        liftIO $ renderToFile f pageHtml

    let bib = "references.bib"
        csl = "templates/modern-language-association.csl"
        template = "templates/template.html"

    "dest/index.html" %> \f -> do
        let source = destToSource f
        need [ source, template ]
        -- Run all org blocks from index.org, to automatically update word counts and so on.
        -- cmd_ "emacs" ["--batch", "--load", "ob", "--load", "ob-shell", "--eval",
        --              "(let ((org-confirm-babel-evaluate nil))(dolist (file command-line-args-left)" ++
        --              "(with-current-buffer (find-file-noselect file)(org-babel-execute-buffer)(save-buffer))))"
        --              , source
        --              ]
        contents <- liftIO $ readFile source
        cmd (Stdin contents) "pandoc" ["-f", "org+smart",
                                       "--template", template,
                                       "--standalone",
                                       "--section-divs",
                                       "--variable=autoSectionLabels:true",
                                       "-o", f
                                       ]


    ["dest//images/*", "dest//includes/*", "dest/assets/**"] |%> \f -> do
        let source = dropDirectory1 f
        need [source]
        copyFileChanged source f

    "dest/00-introduction/introduction.html" %> \f -> do
        assets <- getDirectoryFiles "" [ "00-introduction/images/*"
                                       , "assets//*"
                                       ]
        let outAssets = map ("dest/" <>) assets
        let source = destToSource f
        need ([ source, template, csl, bib ] ++ outAssets)
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
                                       "--filter=templates/PandocSidenote.hs",
                                       "--filter=pandoc-crossref",
                                       "--citeproc",
                                       "--mathjax",
                                       "--bibliography", bib,
                                       "-o", f
                                       ]

    "dest/01-colors/ch-1.html" %> \f -> do
        assets <- getDirectoryFiles "" [ "01-colors/images/*"
                                       , "assets/*/*"
                                       , "01-colors/includes/*" ]
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

    "dest/02-shapes/ch-2.html" %> \f -> do
        assets <- getDirectoryFiles "" [ "02-shapes/images/*"
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

    "dest/03-images/ch-3.html" %> \f -> do
        assets <- getDirectoryFiles "" [ "03-images/images/*"
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
