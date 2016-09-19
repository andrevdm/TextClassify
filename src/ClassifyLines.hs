{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ClassifyLines
  (classifyLineDetail
  ,classifyLineSimple
  ) where

import           Protolude
import qualified Data.Text as Txt
import qualified Data.List as Lst
import           Text.Printf (printf)
import           TfIdf
import           Classify

classifyLine :: TrainedData -> Text -> Text -> ([(Category, Double)], Text, Text )
classifyLine trained origLine cleanedLine = do
  let classified = classifyDetail trained cleanedLine 
  let classifiedNonZero = filter (\(c,v) -> v > 0) classified
  (classifiedNonZero, origLine, cleanedLine)

classifyLineSimple :: TrainedData -> Text -> Text -> Either Text [Text]
classifyLineSimple trained origLine cleanedLine =
  let (cats, t, cl) = classifyLine trained origLine cleanedLine in
  Right $ case Lst.take 1 cats of
            [] -> ["unmatched: " <> t <> " ** " <> cl]
            [(Category c, d)] -> [c <> ": " <> t <> " @" <> showTfIdfVal d <> " ** " <> cl]

classifyLineDetail :: TrainedData -> Text -> Text -> Either Text [Text]
classifyLineDetail trained origLine cleanedLine =
  let (cats, txt, cleaned) = classifyLine trained origLine cleanedLine in
  Right $ [txt
          ," > " <> cleaned
          ] <> (catTxt <$> cats)

  where
    catTxt :: (Category, Double) -> Text
    catTxt (Category cat, tfidf) =
      "  = " <> cat <> ": " <> showTfIdfVal tfidf
  