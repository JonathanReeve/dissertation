#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

-- Transform synsets like /living_thing.n.01/ hexes like #af29b2 into HTML stuff.

import Text.Pandoc.JSON
import Data.Text.ICU.Replace as TR ( replace, replaceAll )
import Data.Text.ICU ()
import Data.Text ( Text )

main :: IO ()
main = toJSONFilter makeSynset

makeSynset :: Inline -> Inline
makeSynset (Code attr s) = RawInline "html" (replaceSynsets s)
makeSynset x = x

-- Replace annotations like sand//sand.n.01 with links to their pages on WordNet Web
-- replaceAnnotated :: Text -> Text
-- replaceAnnotated pat = let searchPat = "\\s(\\w+)//([a-z_]+)\\.[navrs]\\.[0-9]{2}\\s"
--                            lemma = TR.replaceAll "_" "%20" $ TR.replace searchPat "$1" pat
--                            synset = TR.replace searchPat "$2" pat
--                         -- in "1: "<>lemma<>" 2: "<>synset<>" 3: "<>synset'
--                      in "<a class=\"synset\" href=\"http://wordnet-rdf.princeton.edu/lemma/"<>lemma<>"\">"<>synset<>"</a>"

-- Replace synsets like /living_thing.n.01/ with links to their pages on WordNet Web
replaceSynsets :: Text -> Text
replaceSynsets pat = let searchPat = "(([a-z_]+)\\.[navr]\\.[0-9]{2})"
                         lemma = TR.replaceAll "_" "%20" $ TR.replace searchPat "$2" pat
                         synset = TR.replace searchPat "$1" pat
                     in "<a class=\"synset\" href=\"http://wordnet-rdf.princeton.edu/lemma/"<>lemma<>"\">"<>synset<>"</a>"
