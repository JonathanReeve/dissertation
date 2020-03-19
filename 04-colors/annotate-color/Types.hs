{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics

type Span = (Start, End)
type Start = Int
type End = Int

type ColorOrNot = Either Unmatched (ColorFound, ColorStandardized)
type Unmatched = T.Text
type ColorFound = T.Text
type ColorStandardized = T.Text

type ColorMap = M.Map ColorWord Hex
type ColorWord = T.Text
type Hex = T.Text
type Parent = T.Text -- Category

type ColorStatsMap = [(TextName, ColorMapName, [(ColorWord, Hex, Parent, Int, [Span])])]
type TextName = T.Text
type ColorMapName = T.Text

data ColorStats = ColorStatsMap deriving (Generic, ToJSON, FromJSON)
