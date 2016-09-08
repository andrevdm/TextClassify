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
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Vector as V
import qualified Args

classifyCsv :: TrainedData -> Args.Options -> Text -> IO ()
classifyCsv trained opts inputData = do
  let parsed = (decode NoHeader $ BL8.pack (Txt.unpack inputData)) :: Either [Char] (V.Vector [Text])
  case parsed of
    Right csv ->
      case safeHead csv of
        Nothing ->
          pure ()
        Just csvHeader -> do
          writeCsvLine csvHeader
          --classifyCsvLines <$> (V.tail csv)
          pure ()
    Left err ->
      putText $ "CSV error: " <> Txt.pack err

  where
    classifyCsvLines :: [Text] -> IO ()
    classifyCsvLines line = do
      pure ()

    writeCsvLine :: [Text] -> IO ()
    writeCsvLine csv =
      putText $ Txt.intercalate "," csv -- TODO Format

    safeHead :: V.Vector a -> Maybe a
    safeHead v =
      if V.null v then Nothing else Just $ V.head v