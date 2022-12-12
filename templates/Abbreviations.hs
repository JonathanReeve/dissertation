-- |

module Abbreviations where

import Data.Map ( fromList, Map )

abbreviations :: Map String String
abbreviations = fromList [
    ("OED", "Oxford English Dictionary")
  , ("C_{PG}", "Corpus: Project Gutenberg subset")
  , ("C_{PG2}", "Corpus: Project Gutenberg subset, 1800—1922")
  ]
