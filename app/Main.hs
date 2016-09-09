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
  args <- Args.getOptions

  trainingSet <- loadTrainingSet . Txt.unpack $ Args.trainingPath args
  let trained = train trainingSet

  case Txt.toLower $ Args.parserType args of
    "lines" -> CLine.classifyLinesSimple trained args 
    "detail" -> CLine.classifyLinesDetail trained args 
    "csv" -> CCsv.classifyCsv trained args 
    x -> putText $ "unknown parser " <> x