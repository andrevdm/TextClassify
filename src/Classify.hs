{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Classify
  (getWords
  ,classify
  ) where

import Protolude
import Data.Map.Strict (Map)
import qualified Data.Text as Txt
import qualified Data.Map.Strict as Map
import qualified Data.List as Lst
import qualified Args
import TfIdf 

classify :: Args.Options -> TrainedData -> Text -> Maybe (Category, Double)
classify opts trained txt =
  let dupWords = getWords txt in
  let words = Lst.nub dupWords in
  let res = categoriseWords trained words in
  let sorted = sortBy (\(_,va) (_,vb) -> compare vb va) res in
  case sorted of
    top@(c,v) : _ -> if v > 0 then Just top else Nothing
    _ -> Nothing

getWords :: Text -> [Text]
getWords txt = 
  let words = Txt.words txt in
  let nonEmptyWords = filter (not . Txt.null) words in
  Lst.nub nonEmptyWords