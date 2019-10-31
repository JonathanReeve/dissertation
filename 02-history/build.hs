-- Shakefile

import Prelude hiding ((*>))
import Development.Shake

opts = shakeOptions { shakeFiles    = ".shake/" }        -- 1

main :: IO ()
main = shakeArgs opts $ do
    want ["ch-2.pdf"]

    "clean" ~> removeFilesAfter ".shake" ["//*"]

    "ch-2.pdf" %> \f -> do
        need ["ch-2.md",
              "images/1914-isochrone-map.jpg",
              "images/optic-nerve.png",
              "images/richards.png",
              "modern-language-association.csl",
              "references.bib"]
        cmd "pandoc" [ "ch-2.md",
                       "--csl", "modern-language-association.csl",
                       "--pdf-engine", "xelatex",
                       "--filter", "pandoc-citeproc",
                       "-V", "linestretch=1.8",
                       "-V", "fontsize=12p",
                       "-o", f
                     ]

    "ch-2.html.pdf" %> \f -> do
        need ["ch-2.md",
              "images/1914-isochrone-map.jpg",
              "images/optic-nerve.png",
              "images/richards.png",
              "modern-language-association.csl",
              "references.bib",
              "template.html"]
        cmd "pandoc" [ "ch-2.md",
                       "--csl", "modern-language-association.csl",
                       "--pdf-engine", "weasyprint",
                       "--filter", "pandoc-citeproc",
                       "--template", "template.html",
                       "-V", "linestretch=1.8",
                       "-V", "fontsize=12p",
                       "-o", f
                     ]
