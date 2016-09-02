{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

import Protolude
import Data.Map.Strict (Map)
import qualified Data.Text as Txt
import qualified Data.Map.Strict as Map
import qualified Data.List as Lst
import qualified Control.Arrow as Ar 
import qualified Text.Regex as Re
import qualified System.Directory as Dir
import Classify (TrainingSet (..), Category (..))

isValidTxtName :: FilePath -> Bool
isValidTxtName name =
  Txt.takeEnd 4 (Txt.toLower $ Txt.pack name) == ".txt"

getText :: FilePath -> IO (Text, Text)
getText path = do
  txt <- readFile $ "./trainingData/" <> path
  pure (Txt.pack path, txt)

getWords :: (Text, Text) -> (Category, [Text])
getWords (cat, txt) = 
  let words = Txt.words txt in
  let cleanWords = Lst.nub $ cleanWord <$> words in
  let nonEmptyWords = filter (not . Txt.null) cleanWords in
  let category = reverse . drop 4 . reverse $ Txt.unpack cat in
  (Category $ Txt.pack category, Lst.nub nonEmptyWords)

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
  let cleanupPatterns = map (\(f,t) -> (Re.mkRegexWithOpts f True False, t)) cleanupTxt in
  foldl (\s (f,t) -> Txt.strip $ Txt.pack $ Re.subRegex f (Txt.unpack s) t) (Txt.toLower trimmed) cleanupPatterns

loadTrainingSet :: IO TrainingSet
loadTrainingSet = do
  let fs = Dir.getDirectoryContents =<< ((<> "/trainingData") <$> Dir.getCurrentDirectory)
  let efs = filterM (\f -> Dir.doesFileExist $ "./trainingData/" <> f) =<< fs

  let files = filter isValidTxtName <$> efs
  let catText = mapM getText =<< files
  let catWords = mapM (pure . getWords) =<< catText
  TrainingSet <$> catWords
