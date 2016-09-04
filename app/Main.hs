{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Protolude
import           Control.Monad
import           Data.Maybe
import           Data.List ((!!))
import qualified Data.Map as Map
import qualified Data.List as Lst
import qualified Data.Text as Txt
import qualified Control.Arrow as Ar
import qualified Text.Regex as Re
import qualified System.Directory as Dir
import qualified System.Environment as Env
import           Options.Generic
import           Classify
import           ClassifyIO
import           TfIdf

data Options = Options {train :: [Char] <?> "Path to training data"
                       ,input :: [Char] <?> "Input file to categorise"
                       } deriving (Generic, Show)
instance ParseRecord Options

main :: IO ()
main = do
  opts <- getRecord "Demo"
  --putText $ show (opts :: Options)

  trainingSet <- loadTrainingSet $ unHelpful (train opts)

  let trained = buildTfIdf trainingSet
  lines <- Txt.lines <$> readFile (unHelpful $ input opts)

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
