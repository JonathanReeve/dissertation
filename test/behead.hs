#!/usr/bin/env runhaskell
-- behead.hs
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter behead

behead :: Block -> Block
behead (Header n _ xs) | n >= 2 = Para [Emph xs]
behead x = x
