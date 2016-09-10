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
import qualified System.Directory as Dir
import           Classify
import           ClassifyIO
import           TfIdf
import qualified Args
import qualified ClassifyLines as CLine
import qualified ClassifyCsv as CCsv

main :: IO ()
main = do
  opts <- Args.getOptions

  trainingSet <- loadTrainingSet opts . Txt.unpack $ Args.trainingPath opts
  let trained = train trainingSet

  case Txt.toLower $ Args.parserType opts of
    "lines" -> CLine.classifyLinesSimple trained opts 
    "detail" -> CLine.classifyLinesDetail trained opts 
    "csv" -> CCsv.classifyCsv trained opts 
    x -> putText $ "unknown parser " <> x