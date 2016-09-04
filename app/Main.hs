{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Protolude
import Control.Monad
import Data.Maybe
import Data.List ((!!))
import qualified Data.Map as Map
import qualified Data.List as Lst
import qualified Data.Text as Txt
import qualified Control.Arrow as Ar
import qualified Text.Regex as Re
import qualified System.Directory as Dir
import qualified System.Environment as Env
import Classify
import ClassifyIO
import TfIdf


main :: IO ()
main = do
  args <- Env.getArgs
  let trainingPath = args !! 0
  trainingSet <- loadTrainingSet trainingPath

  --putText ".a trainingSet"
  --putText $ show trainingSet
  
  let trained = buildTfIdf trainingSet

  --putText ".b"
  --putText $ show trained

  --putText ".c"
  lines <- Txt.lines <$> readFile "test.txt"
  --putText $ show lines
  --putText "--"

  let res = classify trained $ (\l -> Record () l) <$> lines
  mapM_ (putText . show) res

  where
    txtFromRecord :: Record () -> Text
    txtFromRecord (Record a t) =
      t
    
    showT :: (Show s) => s -> Text
    showT = show

    prn :: TrainedData -> IO ()
    prn t = do
      putText ""
      putText "tf_idf"
      putText . showArr $ tf_Idf t

      putText ""
      putText "tfIdf"
      putText . showArr $ tfIdf t

      putText ""
      putText "idf"
      putText . showT $ idf t

    showArr :: (Show s) => [s] -> Text
    showArr a =
      Txt.intercalate "\n" $ map showT a
