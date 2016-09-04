{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ClassifyLines
  (classifyLines
  ) where

import           Protolude
import qualified Data.Text as Txt
import           TfIdf
import           Classify
import           ClassifyIO
import qualified Args

classifyLines :: Args.Arguments -> IO ()
classifyLines args = do
  trainingSet <- loadTrainingSet . Txt.unpack $ Args.trainingPath args

  let trained = buildTfIdf trainingSet
  lines <- Txt.lines <$> readFile (Txt.unpack $ Args.inputPath args)
  
  let res = (\l -> (classify trained l, l)) <$> lines
  mapM_ prn res

  where
    prn :: (Maybe (Category, Double), Text) -> IO ()
    prn record =
      case record of
        (Nothing, t) -> putText $ t <> ": unmatched"
        (Just (Category c, d), t) -> putText $ c <> ": " <> t <> " @" <> show d