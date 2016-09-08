{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ClassifyLines
  (classifyLines
  ) where

import           Protolude
import qualified Data.Text as Txt
import           System.IO as IO
import           TfIdf
import           Classify
import           ClassifyIO
import qualified Args

classifyLines :: TrainedData -> Args.Options -> IO ()
classifyLines trained opts = do
  contents <- IO.hGetContents (Args.hin opts) 
  let lines = Txt.lines (Txt.pack contents)
  res <- sequenceA $ classifyLine <$> lines
  mapM_ prn res

  where
    classifyLine :: Text -> IO (Maybe (Category, Double), Text, Text )
    classifyLine txt = do
      cleaned <- Args.txtCleaner opts txt 
      let classified = classify opts trained cleaned 
      pure (classified, txt, cleaned)

    prn :: (Maybe (Category, Double), Text, Text) -> IO ()
    prn record =
      case record of
        (Nothing, t, cl) -> putText $ "unmatched: " <> t <> " ** " <> cl
        (Just (Category c, d), t, cl) -> putText $ c <> ": " <> t <> " @" <> show d <> " ** " <> cl
