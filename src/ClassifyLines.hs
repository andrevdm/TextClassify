{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ClassifyLines
  (classifyLinesDetail
  ,classifyLinesSimple
  ) where

import           Protolude
import qualified Data.Text as Txt
import qualified Data.List as Lst
import           System.IO as IO
import           TfIdf
import           Classify
import           ClassifyIO
import qualified Args

classifyLines :: TrainedData -> Args.Options -> IO [([(Category, Double)], Text, Text )]
classifyLines trained opts = do
  contents <- IO.hGetContents (Args.hin opts) 
  let lines = Lst.lines contents
  sequenceA $ classifyLine . Txt.pack <$> lines

  where
    classifyLine :: Text -> IO ([(Category, Double)], Text, Text )
    classifyLine txt = do
      cleaned <- Args.txtCleaner opts txt 
      let classified = classifyDetail opts trained cleaned 
      let classifiedNonZero = filter (\(c,v) -> v > 0) classified

      pure (classifiedNonZero, txt, cleaned)

classifyLinesDetail :: TrainedData -> Args.Options -> IO ()
classifyLinesDetail trained opts = do
  res <- classifyLines trained opts
  traverse_ prn res

  where
    prn :: ([(Category, Double)], Text, Text) -> IO ()
    prn (cats, txt, cleaned) = do
      putText txt
      putText $ "  > " <> cleaned 
      traverse_ prnCategory cats

    prnCategory :: (Category, Double) -> IO ()
    prnCategory (Category cat, tfidf) =
      putText $ "  = " <> cat <> ": " <> show tfidf

classifyLinesSimple :: TrainedData -> Args.Options -> IO ()
classifyLinesSimple trained opts = do
  res <- classifyLines trained opts
  mapM_ prn res

  where
    prn :: ([(Category, Double)], Text, Text) -> IO ()
    prn (cats, t, cl) =
      case Lst.take 1 cats of
        [] -> putText $ "unmatched: " <> t <> " ** " <> cl
        [(Category c, d)] -> putText $ c <> ": " <> t <> " @" <> show d <> " ** " <> cl
