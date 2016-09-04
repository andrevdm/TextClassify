{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TfIdf
  (TrainingSet (..)
  ,Category (..)
  ,TrainedData (..)
  ,train
  ,categorise
  ) where

import Protolude
import Data.Map.Strict (Map)
import qualified Data.Text as Txt
import qualified Data.Map.Strict as Map
import qualified Data.List as Lst
import qualified Control.Arrow as Ar 

newtype Tf = Tf Double deriving (Show)
newtype Idf = Idf Double deriving (Show)
newtype Category = Category Text deriving (Show, Eq, Ord)
newtype Term = Term Text deriving (Show, Eq, Ord)

data TrainingSet = TrainingSet [(Category, [Text])] deriving (Show)

data TrainedData = TrainedData { catTf  :: ![(Category, Map Term Tf)]
                               , tf_Idf :: ![(Category, Map Term (Tf,Idf))]
                               , tfIdf  :: ![(Category, Map Term Double)]
                               , idf    :: !(Map Term Idf)
                               } deriving (Show)

log10 :: Floating a => a -> a
log10 = logBase 10

getCategoryTfs :: TrainingSet -> [(Category, Map Term Tf)]
getCategoryTfs (TrainingSet set) = Ar.second getTf <$> set

-- | Get map of term to term frequency
getTf :: [Text] -> Map Term Tf
getTf ts =
  -- | Count the number of times each term occours
  let freq = Map.fromListWith (+) [(Term t, 1) | t <- ts] in
  -- | Convert the count into a Tf, add 1 to avoid division by zero errors
  (\d -> Tf $ d + 1) <$> freq

-- | Given a [map of term to frequency] get a [map of term to frequency and idf] 
getTfAndIdfMaps :: [(Category, Map Term Tf)] -> [(Category, Map Term (Tf,Idf))]
getTfAndIdfMaps set = Ar.second (getTfAndIdf set) <$> set

getTfAndIdf :: [(Category, Map Term Tf)] -> Map Term Tf -> Map Term (Tf, Idf)
getTfAndIdf set tfMap =
  Map.fromList . fmap (\(term, tf) -> (term, (tf, calcIdf set term))) $ Map.toList tfMap
  
calcIdf :: [(Category, Map Term Tf)] -> Term -> Idf
calcIdf set term =
  -- | get the count of the categories containing the term
  let count = length . filter identity $ Map.member term . snd <$> set in
  Idf $ log10 ((fromIntegral (length set) + 1) / fromIntegral (count + 1))

getTfIdfMaps :: [(Category, Map Term (Tf,Idf))] -> [(Category, Map Term Double)]
getTfIdfMaps xs = Ar.second (combineTfIdf <$>) <$> xs

combineTfIdf :: (Tf, Idf) -> Double
combineTfIdf (Tf t, Idf f) = t * f

train :: TrainingSet -> TrainedData
train set =
  let catTermTf = getCategoryTfs set in
  let catTermTfAndIdf = getTfAndIdfMaps catTermTf in
  TrainedData { catTf = catTermTf
              , tf_Idf = catTermTfAndIdf
              , tfIdf = getTfIdfMaps catTermTfAndIdf
              , idf = Map.fromList $ Ar.second snd <$> (Map.toList . snd =<< catTermTfAndIdf) 
              }

mapPair :: (a -> b) -> [a] -> [(a,b)]
mapPair fn xs = (\x -> (x, fn x)) <$> xs

categorise :: TrainedData -> [Text] -> [(Category, Double)]
categorise trained textTerms =
  let terms = Term <$> textTerms in
  let termFreqs = getTf textTerms in -- Map Term Tf
  let catTf_ = catTf trained in

  let mapTermTfIdf = Map.mapWithKey (\term tf -> combineTfIdf (tf, calcIdf catTf_ term)) termFreqs in

  let termIdfs = mapPair (calcIdf catTf_) terms in 
  let searchTfIdfs = map (Ar.second combineTfIdf) $ Map.toList $ getTfAndIdf catTf_ termFreqs in
  let compared = compareToCategory searchTfIdfs <$> tfIdf trained in
  compared

  where
    compareToCategory :: [(Term, Double)] -> (Category, Map Term Double) -> (Category, Double)
    compareToCategory search (cat, tfMap) =
      let ts = Map.toList tfMap in
      let common = Lst.intersectBy sameTerm ts search in
      let commonV = sum $ snd <$> common in
      let allV = sum (snd <$> ts) + sum  (snd <$> search) in

      (cat, (commonV * 2) / allV)

    sameTerm :: (Term,Double) -> (Term,Double) -> Bool
    sameTerm (at, _) (bt, _) = at == bt
