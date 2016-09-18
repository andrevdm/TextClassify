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
import           Control.Monad.Loops (whileM_)
import qualified System.IO as IO

main :: IO ()
main = do
  opts <- Args.getOptions

  trainingSet <- loadTrainingSet opts . Txt.unpack $ Args.trainingPath opts
  let trained = train trainingSet
  let parser = Txt.toLower $ Args.parserType opts 

  if parser == "csv"
  then do
    h <- IO.hGetLine $ Args.hin opts
    putText . CCsv.createHeader $ Txt.pack h
  else 
    pure ()

  whileM_ (not <$> IO.hIsEOF (Args.hin opts)) $ do
    origChars <- IO.hGetLine $ Args.hin opts
    let origLine = Txt.pack origChars
    parsed <- case parser of
                "lines" -> do 
                  cleanedLine <- Args.txtCleaner opts origLine 
                  pure $ CLine.classifyLineSimple trained origLine cleanedLine 
                "detail" -> do
                  cleanedLine <- Args.txtCleaner opts origLine 
                  pure $ CLine.classifyLineDetail trained origLine cleanedLine 
                "csv" -> 
                  case CCsv.parseCsvLine trained opts origLine of
                    Right (ParsedLine (RawText rawText) csv) -> do
                      cleanedText <- Args.txtCleaner opts rawText
                      pure $ CCsv.categoriseCsvLine trained opts (CleanedLine (RawText rawText) (CleanedText cleanedText) csv)
                    Left err -> 
                      pure $ Left err
                x -> 
                  pure $ Left ("unknown parser " <> x)
    
    case parsed of
      Right lines -> putText $ Txt.intercalate "\n" lines
      Left error -> IO.hPutStrLn stderr $ Txt.unpack error