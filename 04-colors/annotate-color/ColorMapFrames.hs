{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes, TypeApplications, TemplateHaskell, OverloadedStrings #-}

module ColorMap where

import Frames
import Frames.CSV
import Frames.TH (rowGen, RowGen(..))

tableTypes' (rowGen "../data/maps/jaffer/ridgway.tsv")
  { rowTypeName = "ColorMap"
  , columnNames = [ "colorName", "hex" ]
  , separator = "\t" }

-- loadRows :: IO (Frame a)
-- loadRows = inCoreAoS (readTableOpt noHParser "../data/maps/jaffer/ridgway.tsv")

loadMovies :: IO (Frame ColorMap)
loadMovies = inCoreAoS $ readTableOpt colorMapParser "../data/maps/jaffer/ridgway.tsv"
