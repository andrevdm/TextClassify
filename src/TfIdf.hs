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

-- | A term is a single word
newtype Term = Term Text deriving (Show, Eq, Ord)
-- | A category name
newtype Category = Category Text deriving (Show, Eq, Ord)
-- | Term frequency value
newtype Tf = Tf Double deriving (Show)
-- | Inverse document frequency value
newtype Idf = Idf Double deriving (Show)
-- | The combined Tf and Idf value
newtype TfIdf = TfIdf Double deriving (Show)
-- | A document is a map of terms to TfIdf
newtype Document = Document (Map Term TfIdf) deriving (Show)

-- | Data making up the training set
data TrainingSet = TrainingSet [(Category, [Text])] deriving (Show)
-- | The trained data, each category linked to a document
data TrainedData = TrainedData [(Category, Document)] deriving (Show)

-- | Classify words
categoriseWords :: TrainedData -> [Text] -> [(Category, Double)]
categoriseWords (TrainedData trained) words =
  let trainedMaps = tfsFromDoc . snd <$> trained in
  let searchTf = calcTermFreq words in
  let (Document searchTfIdf) = getTfIdf trainedMaps searchTf in
  compareToCategory (Map.toList searchTfIdf) <$> trained

  where 
    -- | Calculate how well terms matches categories
    compareToCategory :: [(Term,TfIdf)] -> (Category, Document) -> (Category, Double)
    compareToCategory searchTfIdf (cat, Document catMap) =
      let catList = Map.toList catMap in

      -- | common words in the category and the search text
      let common = Lst.intersectBy sameTerm catList searchTfIdf in
      let commonV = sum $ valFromTfIdf . snd <$> common in
    
      -- | Sum of all the TfIdf values
      let allV = sum (valFromTfIdf . snd <$> searchTfIdf) + sum (valFromTfIdf . snd <$> catList) in
      
      -- | Similarity = ((common a) + (common b)) / (sum all tfIdf)
      (cat, (commonV * 2) / allV)

    -- | True if the two terms contain the same text
    sameTerm :: (Term, TfIdf) -> (Term, TfIdf) -> Bool
    sameTerm (Term t1, _) (Term t2, _) = 
      t1 == t2

    tfsFromDoc :: Document -> Map Term TfIdf
    tfsFromDoc (Document d) = d

    valFromTfIdf :: TfIdf -> Double
    valFromTfIdf (TfIdf v) = v

-- | Created a TrainedData from a TrainingSet
train :: TrainingSet -> TrainedData
train (TrainingSet trainingSet) =
  let catTf = Ar.second calcTermFreq <$> trainingSet in
  let allTermTf = snd <$> catTf in
  TrainedData $ Ar.second (getTfIdf allTermTf) <$> catTf 

-- | Create a document for terms
getTfIdf :: [Map Term a] -> Map Term Tf -> Document
getTfIdf allTermTfs currentTf =
  Document $ Map.mapWithKey (\term tf -> combineTfAndIdf tf $ calcTermIdf allTermTfs term) currentTf
  where 
    -- | TfIdf = tf * idf
    combineTfAndIdf :: Tf -> Idf -> TfIdf
    combineTfAndIdf (Tf t) (Idf f) = 
      TfIdf $ t * f

-- | Calgulate the term frequency for a collection of words
-- | Tf = occurrence / terms in document.
calcTermFreq :: [Text] -> Map Term Tf
calcTermFreq terms =
  -- | Map of term to number of occurrences
  let freq = Map.fromListWith (+) [(Term t, 1) | t <- terms] in
  -- | Document of Term to freq. Tf = occurrence count / terms in doc
  (\d -> Tf $ d / fromIntegral(length terms)) <$> freq

-- | Claculate the term's inverse document frequency
-- | Idf = (tf + 1) / (number of documents + 1)
-- | + 1 is used to avoid divide by zero
calcTermIdf :: [Map Term a] -> Term -> Idf
calcTermIdf termToTfs term =
  let docsWithTerm = filter identity (Map.member term <$> termToTfs) in
  Idf $ log ((fromIntegral . length $ termToTfs) + 1) / ((fromIntegral . length $ docsWithTerm) + 1)
