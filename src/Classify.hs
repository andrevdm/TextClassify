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
import qualified Text.Regex as Re
import TfIdf 

classify :: TrainedData -> Text -> Maybe (Category, Double)
classify trained txt =
  let dupWords = getWords txt in
  let words = Lst.nub $ cleanWord <$> dupWords in
  let res = categoriseWords trained words in
  let sorted = sortBy (\(_,va) (_,vb) -> compare vb va) res in
  case sorted of
    top@(c,v) : _ -> if v > 0 then Just top else Nothing
    _ -> Nothing

getWords :: Text -> [Text]
getWords txt = 
  let words = Txt.words txt in
  let cleanWords = Lst.nub $ cleanWord <$> words in
  let nonEmptyWords = filter (not . Txt.null) cleanWords in
  Lst.nub nonEmptyWords

--TODO remove
cleanWord :: Text -> Text
cleanWord dirty =
  let cleanupTxt = [ ("^#", "")
                   , ("-", "")
                   , (",", "")
                   , ("^C\\*", "")
                   , ("(\\s)\\s+", "\\1")
                   , ("(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)","")
                   , ("^\\s*\\d\\d$\\s*", "")
                   , ("^[0-9]{2}$", "")
                   , ("^.$", "")
                   , ("^\\s*$", "")
                   ] :: [([Char],[Char])] in

  let trimmed = Txt.strip dirty in
  let cleanupPatterns = fmap (\(f,t) -> (Re.mkRegexWithOpts f True False, t)) cleanupTxt in
  foldl (\s (f,t) -> Txt.strip . Txt.pack $ Re.subRegex f (Txt.unpack s) t) (Txt.toLower trimmed) cleanupPatterns

