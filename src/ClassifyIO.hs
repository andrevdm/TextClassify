{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ClassifyIO
  (loadTrainingSet
  ,getFiles
  ,isValidTxtName
  ) where

import Protolude
import Data.Map.Strict (Map)
import qualified Data.Text as Txt
import qualified Data.Map.Strict as Map
import qualified Data.List as Lst
import qualified System.Directory as Dir
import qualified Control.Arrow as Ar
import TfIdf
import Classify

isValidTxtName :: FilePath -> Bool
isValidTxtName name =
  Txt.takeEnd 4 (Txt.toLower $ Txt.pack name) == ".txt"

getFiles :: FilePath -> IO [FilePath]
getFiles path = do
  fs <- Dir.getDirectoryContents path
  efs <- filterM (\f -> Dir.doesFileExist (path <> "/" <> f)) fs
  pure $ filter isValidTxtName efs

loadTrainingSet :: FilePath -> IO TrainingSet
loadTrainingSet path = do
  files <- getFiles path
  fileText <- sequenceA $ readFile <$> ((\p -> path <> "/" <> p) <$> files)
  let catText = zip (Category . Txt.pack <$> files) fileText
  let catWords = Ar.second getWords <$> catText
  TrainingSet <$> pure catWords
