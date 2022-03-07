{-# LANGUAGE OverloadedStrings #-}

-- Transform color hexes like #af29b2 into HTML color blocks.

import Text.Pandoc.JSON
import Data.Text.ICU.Replace as TR
import Data.Text.ICU
import Data.Text

main = toJSONFilter makeColor

makeColor :: Inline -> Inline
makeColor (Str s) = RawInline "html" (replaceHexes s)
makeColor x = x

-- Replace hex colors that look like #FFA8B5 with background colors to make them more readable.
replaceHexes :: Text -> Text
replaceHexes = TR.replace "(#[0-9a-fA-F]{6})" "<span class=\"colorBlock\" style=\"background-color: $1;\">$1</span>"
