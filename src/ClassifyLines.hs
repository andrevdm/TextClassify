{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ClassifyLines
  (classifyLines
  ) where

import           Protolude
import qualified Data.Text as Txt
import           TfIdf
import           Classify
import           ClassifyIO
import qualified Args

classifyLines :: TrainedData -> Args.Arguments -> Text -> IO ()
classifyLines trained args inputData = do
  let lines = Txt.lines inputData
  let res = (\l -> (classify trained l, l)) <$> lines
  mapM_ prn res

  where
    prn :: (Maybe (Category, Double), Text) -> IO ()
    prn record =
      case record of
        (Nothing, t) -> putText $ "unmatched: " <> t
        (Just (Category c, d), t) -> putText $ c <> ": " <> t <> " @" <> show d