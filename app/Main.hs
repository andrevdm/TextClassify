{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Protolude
import Control.Monad
import qualified Data.Map as Map
import qualified Data.List as Lst
import qualified Data.Text as Txt
import qualified Control.Arrow as Ar
import qualified Text.Regex as Re
import qualified System.Directory as Dir
import Lib
import Classify


main :: IO ()
main = do
  trainingSet <- loadTrainingSet
  --(\(TrainingSet ws) -> mapM_ (putText . show) ws) trainingSet
  putText ""
  
  let trained = buildTfIdf trainingSet

  --line <- getLine
  --putText . show $ categorise trained line

  lines <- Txt.lines <$> readFile "./trainingData/todo"
  let res = map (\l -> (l, categorise trained l)) lines
  mapM_ (putText . show) res

  pure ()
  
  where
    categorise :: TrainedData -> Text -> (Category, Double)
    categorise trained line =
      let dws = snd $ getWords ("", line) in
      let ws = Lst.nub $ cleanWord <$> dws in
      let res = scan trained ws in
      let sorted = sortBy (\(ca,va) (cb,vb) -> compare vb va) res in
      case sorted of
        top@(c,v) : _ -> if v > 0 then top else (Category "", 0.0)
        _ -> (Category "", 0.0)
    
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
