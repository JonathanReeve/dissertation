{-# LANGUAGE OverloadedStrings #-}

-- behead2.hs
import Text.Pandoc.JSON

-- main :: IO ()
-- main = toJSONFilter behead
--   where behead (Citation id pre post mode num hash) | hash >= 2 = Citation id pre post mode num hash
--         behead x = x

main = toJSONFilter suppressSubsequent

suppressSubsequent :: Inline -> Inline
suppressSubsequent (Cite [Citation id pre post mode num hash] [citeStr]) | hash >=2 = (Cite [Citation id pre post SuppressAuthor num hash] [citeStr])
suppressSubsequent x = x
