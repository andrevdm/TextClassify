{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TfIdf2 where

import Protolude
import Data.Map.Strict (Map)
import qualified Data.Text as Txt
import qualified Data.Map.Strict as Map
import qualified Data.List as Lst
import qualified Control.Arrow as Ar 

newtype Document = Document (Map Term Tf) deriving (Show)
newtype Tf = Tf Double deriving (Show)
newtype Idf = Idf Double deriving (Show)
newtype Category = Category Text deriving (Show, Eq, Ord)
newtype Term = Term Text deriving (Show, Eq, Ord)

data TrainingSet = TrainingSet [(Category, [Text])] deriving (Show)
data TrainedData = TrainedData [(Category, Document)] deriving (Show)

train :: TrainingSet -> TrainedData
train (TrainingSet trainingSet) =
  TrainedData $ Ar.second createDocument <$> trainingSet

createDocument :: [Text] -> Document
createDocument terms =
  -- | Map of term to number of occurrences
  let freq = Map.fromListWith (+) [(Term t, 1) | t <- terms] in
  -- | Document of Term to freq. Tf = occurrence count / terms in doc
  Document $ (\d -> Tf $ d / fromIntegral(length terms)) <$> freq

calcIdf :: [Document] -> Term -> Idf
calcIdf documents term =
  let docsWithTerm = filter identity ((\(Document d) -> Map.member term d) <$> documents) in
  Idf $ log ((fromIntegral . length $ documents) + 1) / ((fromIntegral . length $ docsWithTerm) + 1)

combineTfAndIdf :: (Tf, Idf) -> Double
combineTfAndIdf (Tf t, Idf f) = 
  t * f

categorise :: TrainedData -> [Text] -> [(Category, Double)]
categorise trained words = 
  categoriseDocument trained $ createDocument words

categoriseDocument :: TrainedData -> Document -> [(Category, Double)]
categoriseDocument (TrainedData trained) (Document searchDoc) =
  let trainedDocuments = snd <$> trained in
  let searchTermToTfAndIdf = Map.mapWithKey (\term tf -> (tf, calcIdf trainedDocuments term)) searchDoc in
  let searchTermToTfIdf = combineTfAndIdf <$> searchTermToTfAndIdf in
  --let x = Ar.second (calcIdf trainedDocuments) <$> termToTf in
  _
