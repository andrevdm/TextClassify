{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Classify
  (getWords
  ,classify
  ,classifyDetail
  ,showTfIdfVal
  ,ParsedLine (..)
  ,CleanedLine (..)
  ,RawText (..)
  ,CleanedText (..)
  ) where

import Protolude
import Data.Map.Strict (Map)
import qualified Data.Text as Txt
import qualified Data.Map.Strict as Map
import qualified Data.List as Lst
import Text.Printf
import qualified Args
import TfIdf 

-- | A text value that has not been cleaned
newtype RawText = RawText Text deriving (Show)
-- | A text value that has been cleaned. Typically a RawText value becomes a CleanedText value
newtype CleanedText = CleanedText Text deriving (Show)
-- | A line of data containing an uncleaned value. The **'a'** allows the caller to 'store' any additional value e.g. the CSV columns
data ParsedLine a = ParsedLine RawText a deriving (Show)
-- | A line of data containing a cleaned value.
data CleanedLine a = CleanedLine RawText CleanedText a deriving (Show) 

-- | Classify a line of text and try get the best matching category
classify :: Args.Options -> TrainedData -> Text -> Maybe (Category, Double)
classify opts trained txt =
  case classifyDetail trained txt of
    top@(c,v) : _ -> if v > 0 then Just top else Nothing
    _ -> Nothing

-- | Classify a line of text and get all matching categories, best first
classifyDetail :: TrainedData -> Text -> [(Category, Double)]
classifyDetail trained txt =
  let dupWords = getWords txt in
  let words = Lst.nub dupWords in
  let res = categoriseWords trained words in
  sortBy (\(_,va) (_,vb) -> compare vb va) res

-- | Split a line into words
getWords :: Text -> [Text]
getWords txt = 
  let words = Txt.words txt in
  let nonEmptyWords = filter (not . Txt.null) words in
  Lst.nub nonEmptyWords

-- | Display a TfIdf value in a human redable format
showTfIdfVal :: Double -> Text
showTfIdfVal d =
  Txt.pack $ printf "%.4f" d