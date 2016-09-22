{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ClassifyCsv
  (parseCsvLine
  ,categoriseCsvLine
  ,createHeader
  ) where

import           Protolude
import qualified Data.Text as Txt
import           TfIdf
import           Classify
import           Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Vector as V
import           Data.Vector ( (!?) )
import           Text.Printf (printf)
import qualified Args

-- | Add a header column for each of the new columns that will be  added
createHeader :: Text -> Text
createHeader h =
  h <> ",_Category,_TfIdf,_MatchText"

-- | Get a **ParsedLine a** for a line of CSV data. The 'a' is set to the columns
-- | The classification is done in categoriseCsvLines
-- | This is split into two operations so that the IO can be performed outside of this function, no need for it to be in IO
parseCsvLine :: TrainedData -> Args.Options -> Text -> Either Text (ParsedLine [Text])
parseCsvLine trained opts line =
  let contents = BL8.pack . Txt.unpack $ line in
  let parsed = decode NoHeader contents :: Either [Char] (V.Vector [Text]) in
  case parsed of
    Right csv ->
      case safeHead csv of
        Nothing -> Left ""
        Just csvCols -> 
          case getDataCol opts of
            Just dataIdx -> 
              case take 1 . drop (dataIdx - 1) $ csvCols of
                [val] -> 
                  Right $ ParsedLine (RawText val) csvCols
                _ -> 
                  Left $ "No column at index" <> show dataIdx
            Nothing ->
               Left "* No data column specified in the potps option"
    Left err ->
      Left $ "CSV error: " <> Txt.pack err

-- | Given a cleaned line, classify the text
categoriseCsvLine :: TrainedData -> Args.Options -> CleanedLine [Text] -> Either Text [Text]
categoriseCsvLine trained opts (CleanedLine (RawText origText) (CleanedText cleanedText) csvRows) =
  let classified = classify opts trained cleanedText in
  Right [case classified of
           Just (Category cat, tfidf) -> 
             createCsvLine (csvRows <> [cat, showTfIdfVal tfidf, cleanedText])
           Nothing -> 
             createCsvLine (csvRows <> ["-", "0", cleanedText])
        ]
        
-- | Create a CSV output line. Cassava's encode is used to encode the CSV
createCsvLine :: [Text] -> Text
createCsvLine csv = 
  Txt.stripEnd . Txt.pack . BL8.unpack . encode $ [Txt.strip <$> csv]

-- | Get the head of a vector if there is one
safeHead :: V.Vector a -> Maybe a
safeHead v =
  if V.null v then Nothing else Just $ V.head v

-- | Get the Int value of optStr, i.e. the column index in the CSV to use as the text to classify 
getDataCol :: Args.Options -> Maybe Int
getDataCol opts =
  case Args.parserOptions opts of
    Nothing -> 
      Nothing
    Just optsStr ->
      case (reads . Txt.unpack $ optsStr :: [(Int, [Char])]) of
        [(col,[])] -> Just col
        _ -> Nothing