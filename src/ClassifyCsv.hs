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

createHeader :: Text -> Text
createHeader h =
  h <> "_Category,_TfIdf,_MatchText"

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

categoriseCsvLine :: TrainedData -> Args.Options -> CleanedLine [Text] -> Either Text [Text]
categoriseCsvLine trained opts (CleanedLine (RawText origText) (CleanedText cleanedText) csvRows) =
  let classified = classify opts trained cleanedText in
  Right [case classified of
           Just (Category cat, tfidf) -> 
             createCsvLine (csvRows <> [cat, showTfIdfVal tfidf, cleanedText])
           Nothing -> 
             createCsvLine (csvRows <> ["-", "0", cleanedText])
        ]
        
createCsvLine :: [Text] -> Text
createCsvLine csv = 
  Txt.stripEnd . Txt.pack . BL8.unpack . encode $ [Txt.strip <$> csv]

safeHead :: V.Vector a -> Maybe a
safeHead v =
  if V.null v then Nothing else Just $ V.head v

getDataCol :: Args.Options -> Maybe Int
getDataCol opts =
  case Args.parserOptions opts of
    Nothing -> 
      Nothing
    Just optsStr ->
      case (reads . Txt.unpack $ optsStr :: [(Int, [Char])]) of
        [(col,[])] -> Just col
        _ -> Nothing