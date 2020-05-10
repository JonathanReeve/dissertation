{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

maps :: [FilePath]
maps = [ -- "../data/maps/pantone/pantone.tsv"
        "../data/maps/xkcd/rgb.txt"
       -- "../data/maps/jaffer/master.tsv"
       ]

tsvToPat tsv = T.unlines $ map (makePat . (T.replace "-" " ") . head . (T.splitOn "\t")) (T.lines tsv)

makePat :: T.Text -> T.Text
makePat pat = T.concat [ "{\"label\":\"COLOR\",\"pattern\":[{\"lower\":\""
                       , pat
                       , "\"}]}"
                       ]

main = mapM_ (TIO.putStr . tsvToPat <=< TIO.readFile) maps
