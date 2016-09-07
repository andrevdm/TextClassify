{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TfIdf 
  (categoriseWords
  ,train
  ,Category (..)
  ,TrainingSet (..)
  ,TrainedData (..)
  )where

import Protolude
import Data.Map.Strict (Map)
import qualified Data.Text as Txt
import qualified Data.Map.Strict as Map
import qualified Data.List as Lst
import qualified Control.Arrow as Ar 

newtype Document = Document (Map Term TfIdf) deriving (Show)
newtype TfIdf = TfIdf Double deriving (Show)
newtype Tf = Tf Double deriving (Show)
newtype Idf = Idf Double deriving (Show)
newtype Category = Category Text deriving (Show, Eq, Ord)
newtype Term = Term Text deriving (Show, Eq, Ord)

data TrainingSet = TrainingSet [(Category, [Text])] deriving (Show)
data TrainedData = TrainedData [(Category, Document)] deriving (Show)

categoriseWords :: TrainedData -> [Text] -> [(Category, Double)]
categoriseWords (TrainedData trained) words =
  let trainedMaps = tfsFromDoc . snd <$> trained in
  let searchTf = calcTermFreq words in
  let (Document searchTfIdf) = getTfIdf trainedMaps searchTf in
  compareToCategory (Map.toList searchTfIdf) <$> trained

  where 
    compareToCategory :: [(Term,TfIdf)] -> (Category, Document) -> (Category, Double)
    compareToCategory searchTfIdf (cat, Document catMap) =
      let catList = Map.toList catMap in
      let common = Lst.intersectBy sameTerm catList searchTfIdf in
      let commonV = sum $ valFromTfIdf . snd <$> common in
      let allV = sum (valFromTfIdf . snd <$> searchTfIdf) + sum (valFromTfIdf . snd <$> catList) in
      (cat, (commonV * 2) / allV)

    sameTerm :: (Term, TfIdf) -> (Term, TfIdf) -> Bool
    sameTerm (Term t1, _) (Term t2, _) = 
      t1 == t2

    tfsFromDoc :: Document -> Map Term TfIdf
    tfsFromDoc (Document d) = d

    valFromTfIdf :: TfIdf -> Double
    valFromTfIdf (TfIdf v) = v

train :: TrainingSet -> TrainedData
train (TrainingSet trainingSet) =
  let catTf = Ar.second calcTermFreq <$> trainingSet in
  let allTermTf = snd <$> catTf in
  TrainedData $ Ar.second (getTfIdf allTermTf) <$> catTf 

getTfIdf :: [Map Term a] -> Map Term Tf -> Document
getTfIdf allTermTfs currentTf =
  Document $ Map.mapWithKey (\term tf -> combineTfAndIdf tf $ calcTermIdf allTermTfs term) currentTf
  where 
    combineTfAndIdf :: Tf -> Idf -> TfIdf
    combineTfAndIdf (Tf t) (Idf f) = 
      TfIdf $ t * f

calcTermFreq :: [Text] -> Map Term Tf
calcTermFreq terms =
  -- | Map of term to number of occurrences
  let freq = Map.fromListWith (+) [(Term t, 1) | t <- terms] in
  -- | Document of Term to freq. Tf = occurrence count / terms in doc
  (\d -> Tf $ d / fromIntegral(length terms)) <$> freq

calcTermIdf :: [Map Term a] -> Term -> Idf
calcTermIdf termToTfs term =
  let docsWithTerm = filter identity (Map.member term <$> termToTfs) in
  Idf $ log ((fromIntegral . length $ termToTfs) + 1) / ((fromIntegral . length $ docsWithTerm) + 1)
