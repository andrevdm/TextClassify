{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Args 
       (Options (..)
       ,getOptions
       ) where

import           Protolude
import           Data.Maybe
import qualified Data.Text as Txt
import           System.IO
import           System.Process
import           Options.Generic

data Arguments = Arguments {train :: [Char] <?> "Path to training data"
                           ,input :: Maybe [Char] <?> "Input file to categorise. If missing stdin will be used"
                           ,parser :: Maybe Text <?> "Parser type, defaults to text"
                           ,popts :: Maybe Text <?> "Parser options"
                           ,clean :: Maybe Text <?> "Options name of text cleander - see docs"
                           } deriving (Generic, Show)
instance ParseRecord Arguments

data Options = Options {trainingPath :: Text
                       ,inputPath :: Maybe Text
                       ,parserType :: Text
                       ,parserOptions :: Maybe Text
                       ,txtCleaner :: Text -> IO Text
                       } 

getOptions :: IO Options
getOptions = do
  args <- getRecord "TextClassifierArgs"
  cleaner <- getCleaner (unHelpful (clean args)) 
  pure Options {trainingPath = Txt.pack $ unHelpful (train args)
               ,inputPath = case unHelpful $ input args of
                              Just t -> Just $ Txt.pack t
                              Nothing -> Nothing
               ,parserType = fromMaybe "lines" $ unHelpful (parser args)
               ,parserOptions = unHelpful (popts args)
               ,txtCleaner = cleaner
               }
  where
    getCleaner :: Maybe Text -> IO (Text -> IO Text)
    getCleaner mcmd = do
      case mcmd of
        Just cmd -> do
          (Just inp, Just outp, _, phandle) <- createProcess (proc (Txt.unpack cmd) []) { std_out = CreatePipe, std_in = CreatePipe }
          hSetBuffering outp NoBuffering
          hSetBuffering inp NoBuffering
          pure $ cleanText inp outp
        Nothing ->
          pure $ pure . Txt.toLower

    cleanText :: Handle -> Handle -> Text -> IO Text
    cleanText inp outp txt = do
      hPutStrLn inp $ Txt.unpack (Txt.toLower txt)
      pure . Txt.pack =<< hGetLine outp
