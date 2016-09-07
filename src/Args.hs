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
import qualified Data.Text as Txt
import           Options.Generic

data Options = Options {train :: [Char] <?> "Path to training data"
                       ,input :: Maybe [Char] <?> "Input file to categorise. If missing stdin will be used"
                       ,parser :: Maybe Text <?> "Parser type, defaults to text"
                       ,popts :: Maybe Text <?> "Parser options"
                       } deriving (Generic, Show)
instance ParseRecord Options

data Arguments = Arguments {trainingPath :: Text
                           ,inputPath :: Maybe Text
                           ,parserType :: Text
                           ,parserOptions :: Maybe Text
                           } deriving (Show)

getArguments :: IO Arguments
getArguments = do
  opts <- getRecord "TextClassifier"
  pure Arguments {trainingPath = Txt.pack $ unHelpful (train opts)
                 ,inputPath = case unHelpful $ input opts of
                                Just t -> Just $ Txt.pack t
                                Nothing -> Nothing
                 ,parserType = fromMaybe "lines" $ unHelpful (parser opts)
                 ,parserOptions = unHelpful (popts opts)
                 }