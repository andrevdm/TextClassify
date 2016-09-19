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

newtype RawText = RawText Text deriving (Show)
newtype CleanedText = CleanedText Text deriving (Show)
data ParsedLine a = ParsedLine RawText a deriving (Show)
data CleanedLine a = CleanedLine RawText CleanedText a deriving (Show) 

classify :: Args.Options -> TrainedData -> Text -> Maybe (Category, Double)
classify opts trained txt =
  case classifyDetail trained txt of
    top@(c,v) : _ -> if v > 0 then Just top else Nothing
    _ -> Nothing

classifyDetail :: TrainedData -> Text -> [(Category, Double)]
classifyDetail trained txt =
  let dupWords = getWords txt in
  let words = Lst.nub dupWords in
  let res = categoriseWords trained words in
  sortBy (\(_,va) (_,vb) -> compare vb va) res

getWords :: Text -> [Text]
getWords txt = 
  let words = Txt.words txt in
  let nonEmptyWords = filter (not . Txt.null) words in
  Lst.nub nonEmptyWords

showTfIdfVal :: Double -> Text
showTfIdfVal d =
  Txt.pack $ printf "%.4f" d