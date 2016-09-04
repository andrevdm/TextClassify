{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude
import           Control.Monad
import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.List as Lst
import qualified Data.Text as Txt
import qualified Control.Arrow as Ar
import qualified Text.Regex as Re
import qualified System.Directory as Dir
import           Classify
import           ClassifyIO
import           TfIdf
import qualified Args

main :: IO ()
main = do
  opts <- Args.getArguments

  trainingSet <- loadTrainingSet . Txt.unpack $ Args.trainingPath opts

  let trained = buildTfIdf trainingSet
  lines <- Txt.lines <$> readFile (Txt.unpack $ Args.inputPath opts)

  let res = classify trained $ Record () <$> lines
  mapM_ (putText . show) res

  where
    txtFromRecord :: Record () -> Text
    txtFromRecord (Record a t) =
      t

    showT :: (Show s) => s -> Text
    showT = show

    prn :: TrainedData -> IO ()
    prn t = do
      putText ""
      putText "tf_idf"
      putText . showArr $ tf_Idf t

      putText ""
      putText "tfIdf"
      putText . showArr $ tfIdf t

      putText ""
      putText "idf"
      putText . showT $ idf t

    showArr :: (Show s) => [s] -> Text
    showArr a =
      Txt.intercalate "\n" $ showT <$> a
