{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                        (join)
import           Control.Applicative                  ((<$>))
import           Data.Maybe                           (fromMaybe)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           System.Environment                   (lookupEnv)
import           Text.Read                            (readMaybe)
import           Web.Scotty                           (middleware, scotty, get, param, html)

main :: IO ()
main = do
  port <- fromMaybe 3000
    . join
    . fmap readMaybe <$> lookupEnv "PORT"
  scotty port $ do
    -- middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
    middleware logStdoutDev
    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
