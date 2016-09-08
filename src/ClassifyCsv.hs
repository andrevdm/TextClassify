{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ClassifyCsv
  (classifyCsv
  ) where

import           Protolude
import qualified Data.Text as Txt
import           TfIdf
import           Classify
import           ClassifyIO
import           Data.Csv
import           System.IO
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Vector as V
import           Data.Vector ( (!?) )
import qualified Args

classifyCsv :: TrainedData -> Args.Options -> IO ()
classifyCsv trained opts = do
  contents <- BL.hGetContents (Args.hin opts)
  let parsed = decode NoHeader contents :: Either [Char] (V.Vector [Text])
  case parsed of
    Right csv ->
      case safeHead csv of
        Nothing -> pure ()
        Just csvHeader -> classifyCsvLines csvHeader $ V.tail csv
    Left err ->
      hPutStrLn stderr $ "CSV error: " <> err

  where
    classifyCsvLines :: [Text] -> V.Vector [Text] -> IO ()
    classifyCsvLines csvHeader rest = do
      writeCsvLine (csvHeader <> ["_Category", "_TfIdf", "_MatchText"])
      case getDataCol opts of
        Just dataIdx -> mapM_ (decorateAndWriteCsvLine dataIdx) rest
        Nothing -> hPutStrLn stderr "* No data column specified in the potps option"
    
    decorateAndWriteCsvLine :: Int -> [Text] -> IO ()
    decorateAndWriteCsvLine dataIdx line = do
      case take 1 . drop (dataIdx - 1) $ line of
        [val] -> do
          cleaned <- Args.txtCleaner opts val
          let classified = classify opts trained cleaned 
          case classified of
            Just (Category cat, tfidf) -> 
              writeCsvLine (line <> [cat, show tfidf, cleaned])
            Nothing -> 
              writeCsvLine (line <> ["", "0", cleaned])
        _ ->
          hPutStrLn stderr $ "No column at index" <> show dataIdx

    writeCsvLine :: [Text] -> IO ()
    writeCsvLine csv =
      putText $ Txt.intercalate "," csv -- TODO Format

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