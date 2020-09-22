-- Shakefile Stuff
import Development.Shake
import Text.Regex
import qualified Data.Text as T
import Data.Text.ICU (regex)
-- import Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified Data.Text.ICU.Replace as TR

import Main.Utf8 (withUtf8)

import Lucid (renderToFile)

import Template

opts = shakeOptions { shakeFiles    = ".shake/" }

images = getDirectoryFiles "" [ "02-history/images/*" ]

-- Replace org style citation syntax with @-syntax for Pandoc
replaceCites :: T.Text -> T.Text
replaceCites text = TR.replaceAll (regex [] (T.pack "cite:")) (TR.rstring "@") text

readFileText text = need [text] >> liftIO (TIO.readFile text)

main :: IO ()
main = withUtf8 $ shakeArgs opts $ do
    want ["02-history/ch-2.pdf" ]
   -- , "02-history/ch-2.docx"]

    phony "clean" $ do
        removeFilesAfter "02-history" ["/*.log",
                                      "/*.out",
                                      "/*.tex",
                                      "/*.toc",
                                      "/*.run.xml",
                                      "/*.aux",
                                      "/*.bbl",
                                      "/*.bcf",
                                      "/*.blg",
                                      "/references.bib"]

    "//references.bib" %> \f -> do
        let source = "/home/jon/Dokumentujo/Papers/library.bib"
        cmd "cp" source f

    "02-history/ch-2.tex" %> \f -> do
        deps <- images
        let source = "02-history/ch-2.org"
            template = "templates/chapter.tex"
            bib = "references.bib"
            csl = "templates/modern-language-association.csl"
        need ([ source, template, csl, bib ] ++ deps)
        contents <- readFileText source
        let replaced = T.unpack $ replaceCites contents
        cmd (Stdin replaced) "pandoc" ["-f", "org+smart",
                                       "--template", template,
                                       "--standalone",
                                       "--biblatex",
                                       "--bibliography", bib,
                                       "-o", f
                                       ]

    -- Do this using --bibtex instead
    "02-history/ch-2.pdf" %> \f -> do
        let dir = Cwd "02-history"
        let source = "ch-2"
        let bibliography = "references.bib"
        need [ "02-history/ch-2.tex", "02-history/references.bib" ]
        -- cmd_ dir "tectonic" "ch-2.tex"
        cmd_ dir "xelatex" source
        cmd_ dir "biber" source
        cmd_ dir "xelatex" source
        cmd_ dir "xelatex" source

    "02-history/ch-2.docx" %> \f -> do
        let source = "ch-2.tex"
            bib = "02-history/references.bib"
            csl = "../templates/modern-language-association.csl"
        need [ "02-history/ch-2.tex" ]
        cmd_ (Cwd "02-history") "pandoc" [ source,
                                           "-f", "latex",
                                           "--standalone",
                                           "--toc",
                                           "--bibliography", bib,
                                           "--csl", csl,
                                           "--filter", "pandoc-citeproc",
                                           "-o", "ch-2.docx"
                                         ]

    "04-colors/ch-4.html" %> \f -> do
        deps <- images
        let source = "04-colors/ch-4.org"
            template = "templates/template.html"
            bib = "references.bib"
            csl = "templates/modern-language-association.csl"
        need ([ source, template, csl, bib ] ++ deps)
        -- need ([ source ] ++ deps)
        contents <- readFileText source
        let replaced = T.unpack $ replaceCites contents
        cmd (Stdin replaced) "pandoc" ["-f", "org+smart",
                                       "--template", template,
                                       "--standalone",
                                       "--reference-location=block",
                                       "--toc",
                                       "--section-divs",
                                       "--csl=" ++ csl,
                                       "--variable=autoSectionLabels:true",
                                       "--metadata=tblPrefix:table",
                                       "--filter=PandocSidenote.hs",
                                       "--filter=pandoc-crossref",
                                       "--filter=pandoc-citeproc",
                                       "--filter=hex-filter.hs",
                                       "--mathjax",
                                       "--bibliography", bib,
                                       "-o", f
                                       ]

    "templates/template.html" %> \f -> do
        need ["Template.hs"]
        liftIO $ renderToFile f pageHtml
