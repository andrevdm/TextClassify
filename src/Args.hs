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

-- | Arguments parsed py optoparse-generic
data Arguments = Arguments {train :: Text <?> "Path to training data"
                           ,input :: Maybe Text <?> "Input file to categorise. If missing stdin will be used"
                           ,parser :: Maybe Text <?> "Parser type, defaults to lines. Options are lines/detail/csv"
                           ,popts :: Maybe Text <?> "Parser options"
                           ,clean :: Maybe Text <?> "Options name of text cleaner - see docs"
                           } deriving (Generic, Show)
instance ParseRecord Arguments

-- | Intepreted values based on Arguments. E.g. no "helpful" types
data Options = Options {trainingPath :: Text
                       ,parserType :: Text
                       ,parserOptions :: Maybe Text
                       ,txtCleaner :: Text -> IO Text
                       ,hin :: Handle
                       ,hout :: Handle
                       } 

-- | Get the command line arguments
getOptions :: IO Options
getOptions = do
  args <- getRecord "txtcls - Text Classifier. Version 0.1.2"
  cleaner <- getCleaner (unHelpful (clean args)) 
  hin_ <- case unHelpful $ input args of
             Just t -> 
               openFile (Txt.unpack t) ReadMode
             Nothing ->
                pure stdin

  pure Options {trainingPath = unHelpful (train args)
               ,parserType = fromMaybe "lines" $ unHelpful (parser args)
               ,parserOptions = unHelpful (popts args)
               ,txtCleaner = cleaner
               ,hout = stdout
               ,hin = hin_
               }
  where
    -- | Build a 'cleaner'
    getCleaner :: Maybe Text -> IO (Text -> IO Text)
    getCleaner mcmd = 
      case mcmd of
        -- | The cleaner uses the extenal process to do the actual cleaning. One line is writtent to the processes' stdin and then a value is read from its stdout 
        Just cmd -> do
          (Just inp, Just outp, _, phandle) <- createProcess (proc (Txt.unpack cmd) []) { std_out = CreatePipe, std_in = CreatePipe }
          hSetBuffering outp NoBuffering
          hSetBuffering inp LineBuffering
          pure $ cleanText inp outp
        -- | No external cleaner. Just make the text lower case
        Nothing ->
          pure $ pure . Txt.toLower

    -- | Used by getCleaner to build a curried cleaner function
    cleanText :: Handle -> Handle -> Text -> IO Text
    cleanText inp outp txt = do
      hPutStrLn inp $ Txt.unpack (Txt.toLower txt)
      pure . Txt.pack =<< hGetLine outp
