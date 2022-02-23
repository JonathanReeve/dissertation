-- Shakefile Stuff
import Development.Shake
import Text.Regex
import qualified Data.Text as T
import Data.Text.ICU (regex)
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

opts = shakeOptions { shakeFiles    = ".shake/" }

images = getDirectoryFiles "" [ "02-history/images/*" ]

-- Replace org style citation syntax with @-syntax for Pandoc.
-- Why? cite: syntax is much better supported in emacs, and allows me to see
-- at a glance which citations aren't working, and to jump to their entries.
-- But cite: syntax isn't that well supported in Pandoc, so we use Pandoc's
-- "Berkeley-style" citation syntax, instead.
replaceCites :: T.Text -> T.Text
replaceCites text = TR.replaceAll (regex [] (T.pack "cite:@?")) (TR.rstring "@") text

readFileText text = need [text] >> liftIO (TIO.readFile text)

main :: IO ()
main = withUtf8 $ shakeArgs opts $ do
    want [ "index.html"
         , "02-history/ch-2.html"
         , "04-colors/ch-4.html"
         ]

    -- To serve the generated files (useful for previewing),
    -- run `shake serve`.
    phony "serve" $
      liftIO $ serve 8080 "."

    "//references.bib" %> \f -> do
        let source = "/home/jon/Dokumentujo/Papers/library.bib"
        need [source]
        cmd "cp" source f

    "templates/template.html" %> \f -> do
        need ["Template.hs"]
        liftIO $ renderToFile f pageHtml

    "index.html" %> \f -> do
        let source = "index.org"
            template = "templates/template.html"
        need ([ source, template ])
        contents <- liftIO $ readFile source
        cmd (Stdin contents) "pandoc" ["-f", "org+smart",
                                       "--template", template,
                                       "--standalone",
                                       "--section-divs",
                                       "--variable=autoSectionLabels:true",
                                       "-o", f
                                       ]

    let bib = "references.bib"
        csl = "templates/modern-language-association.csl"
        template = "templates/template.html"

    "02-history/ch-2.html" %> \f -> do
        deps <- images
        let source = "02-history/ch-2.org"
        need ([ source, template, csl, bib ] ++ deps)
        contents <- readFileText source
        let replaced = T.unpack $ replaceCites contents
        cmd (Stdin replaced) "pandoc" ["-f", "org+smart",
                                       "--template", template,
                                       "--standalone",
                                       "--section-divs",
                                       "--reference-location=block",
                                       "--csl=" ++ csl,
                                       "--variable=autoSectionLabels:true",
                                       "--metadata=tblPrefix:table",
                                       "--filter=templates/PandocSidenote.hs",
                                       "--filter=pandoc-crossref",
                                       "--citeproc",
                                       "--filter=templates/hex-filter.hs",
                                       "--mathjax",
                                       "--bibliography", bib,
                                       "-o", f
                                       ]

    "04-colors/ch-4.html" %> \f -> do
        deps <- images
        let source = "04-colors/ch-4.org"
            filters = [ "templates/PandocSidenote.hs"
                      , "templates/hex-filter.hs"
                      ]
        need ([ source, template, csl, bib ] ++ deps ++ filters)
        contents <- readFileText source
        let replaced = T.unpack $ replaceCites contents
        cmd (Stdin replaced) "pandoc" ["-f", "org+smart",
                                       "--template", template,
                                       "--standalone",
                                       "--section-divs",
                                       "--reference-location=block",
                                       "--csl=" ++ csl,
                                       "--variable=autoSectionLabels:true",
                                       "--metadata=tblPrefix:table",
                                       "--filter=templates/PandocSidenote.hs",
                                       "--filter=pandoc-crossref",
                                       "--citeproc",
                                       "--filter=templates/hex-filter.hs",
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
