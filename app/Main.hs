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
  -- | Get the parsed command line options
  opts <- Args.getOptions


  -- | Load the training set
  trainingSet <- loadTrainingSet opts . Txt.unpack $ Args.trainingPath opts
  -- | Get the trained data from the training set
  let trained = train trainingSet

  -- | Get the name of the selected parser
  let parser = Txt.toLower $ Args.parserType opts 

  -- | The CSV parser need to read the first line as the header before the remainder of the input is classified 
  if parser == "csv"
  then do
    h <- IO.hGetLine $ Args.hin opts
    putText . CCsv.createHeader $ Txt.pack h
  else 
    pure ()

  -- | Read input a line at a time and pass it to the parser
  whileM_ (not <$> IO.hIsEOF (Args.hin opts)) $ do
    -- | line of data
    origChars <- IO.hGetLine $ Args.hin opts
    let origLine = Txt.pack origChars
    
    -- | parse the line and get the results to display
    parsed <- case parser of
                "lines" -> do 
                  cleanedLine <- Args.txtCleaner opts origLine 
                  pure $ CLine.classifyLineSimple trained origLine cleanedLine 
                "detail" -> do
                  cleanedLine <- Args.txtCleaner opts origLine 
                  pure $ CLine.classifyLineDetail trained origLine cleanedLine 
                "csv" -> 
                  -- | The CSV parser workds in two steps
                  -- | 1. Parse the CSV and get the text to classify 
                  -- | 2. Classify the cleaned text
                  case CCsv.parseCsvLine trained opts origLine of
                    Right (ParsedLine (RawText rawText) csv) -> do
                      cleanedText <- Args.txtCleaner opts rawText
                      pure $ CCsv.categoriseCsvLine trained opts (CleanedLine (RawText rawText) (CleanedText cleanedText) csv)
                    Left err -> 
                      pure $ Left err
                x -> 
                  pure $ Left ("unknown parser " <> x)
    
    -- | Display result or error
    case parsed of
      Right lines -> putText $ Txt.intercalate "\n" lines
      Left error -> IO.hPutStrLn stderr $ Txt.unpack error