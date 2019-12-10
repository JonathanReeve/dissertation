-- Shakefile

import Development.Shake
import Text.Regex

opts = shakeOptions { shakeFiles    = ".shake/" }

images = getDirectoryFiles "" [ "02-history/images/*" ]

replace str = subRegex (mkRegex "cite:") str "@"

main :: IO ()
main = shakeArgs opts $ do
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

    "02-history/references.bib" %> \f -> do
        let source = "/home/jon/Dropbox/Papers/library.bib"
        cmd "cp" source f

    "02-history/ch-2.tex" %> \f -> do
        deps <- images
        let source = "02-history/ch-2.org"
            template = "templates/chapter.tex"
            bib = "references.bib"
            csl = "templates/modern-language-association.csl"
        need ([ source, template, csl, bib ] ++ deps)
        contents <- readFile' source
        let replaced = replace contents
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
