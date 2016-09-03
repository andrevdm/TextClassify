{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module ClassifyIO
  (loadTrainingSet
  ,getFiles
  ) where

import Protolude
import Data.Map.Strict (Map)
import qualified Data.Text as Txt
import qualified Data.Map.Strict as Map
import qualified Data.List as Lst
import qualified System.Directory as Dir
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
  let files = getFiles path
  let catText = mapM getText =<< files
  let catWords = mapM (pure . getWords) =<< catText
  TrainingSet <$> catWords
