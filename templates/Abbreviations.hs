-- |

module Abbreviations where

import Data.Map ( fromList, Map )

abbreviations :: Map String String
abbreviations = fromList [
    ("OED", "Oxford English Dictionary")
  , ("C_{PG}", "Corpus: Project Gutenberg subset")
  , ("C_{PG2}", "Corpus: Project Gutenberg subset, 1800—1922")
  , ("LCSH", "Library of Congress Subject Heading")
  , ("LCC", "Library of Congress Category")
  , ("TEI", "Text Encoding Initiative")
  , ("XML", "Extensible Markup Language")
  , ("OCR", "Optical Character Recognition")
  , ("API", "Application Programming Interface")
  , ("SPARQL", "SPARQL Protocol and RDF Query Language")
  , ("RDF", "Resource Description Framework")
  ]
