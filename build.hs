-- Shakefile

import Development.Shake

opts = shakeOptions { shakeFiles    = ".shake/" }

images = getDirectoryFiles "" [ "02-history/images/*" ]

main :: IO ()
main = shakeArgs opts $ do
    want ["02-history/ch-2.pdf"]

    "02-history/ch-2.pdf" %> \f -> do
        deps <- images
        let source = "02-history/ch-2.md"
        need ([ source,
              "templates/chapter.tex",
              "templates/modern-language-association.csl",
              "references.bib"] ++ deps)
        cmd "pandoc" [ source,
                       "--csl", "templates/modern-language-association.csl",
                       "--pdf-engine", "xelatex",
                       "--filter", "pandoc-citeproc",
                       "--template", "templates/chapter.tex",
                       -- "--biblatex",
                       "-V", "linestretch=1.8",
                       "-V", "fontsize=12p",
                       "-o", f
                     ]

    -- Do this using --bibtex instead? 
    -- "02-history/ch-2.pdf" %> \f -> do
    --     let source = "02-history/ch-2.tex"
    --     let bibliography = "references.bib"
    --     need [ source ]
    --     cmd "xelatex" [ source ]
    --     cmd "bibtex" [ bibliography ]
    --     cmd "xelatex" [ source ]
    --     cmd "xelatex" [ source ]

    "clean" ~> removeFilesAfter ".shake" ["//*"]
