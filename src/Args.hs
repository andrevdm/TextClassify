{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Args 
       (Arguments (..)
       ,getArguments
       ) where

import           Protolude
import           Data.Maybe
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
                       ,parser :: Maybe Text <?> "Parser type, defaults to text"
                       } deriving (Generic, Show)
instance ParseRecord Options

data Arguments = Arguments {trainingPath :: Text
                           ,inputPath :: Text
                           ,parserType :: Text
                           } deriving (Show)

getArguments :: IO Arguments
getArguments = do
  opts <- getRecord "TextClassifier"
  pure Arguments {trainingPath = Txt.pack $ unHelpful (train opts)
                 ,inputPath = Txt.pack $ unHelpful (input opts)
                 ,parserType = ""
                 } 
                          