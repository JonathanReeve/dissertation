{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Turtle hiding ((</>), FilePath)
import ClassyPrelude
import Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

db = "/run/media/jon/Sekurkopioj/Corpora" :: FilePath

str = "Hello!"
main = do
  echo "Starting"
  ids <- TIO.readFile $ db </> "/ids"
  let chunks = chunksOf 20 (lines ids)
  let cmd = T.concat ["sqlite3 ", db, "/pg-text-7.db -batch ",
                      "'select text from text where id=",
                      T.pack id, "';"]
  let outFile = T.concat ["/tmp/", id]
  output outFile (inshell cmd empty)
