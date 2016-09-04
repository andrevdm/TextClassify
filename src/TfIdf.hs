{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TfIdf
  (TrainingSet (..)
  ,Category (..)
  ,TrainedData (..)
  ,buildTfIdf
  ,scan
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

getTf :: [Text] -> Map Term Tf
getTf ts =
  let freq = Map.fromListWith (+) [(Term t, 1) | t <- ts] in
  Map.fromList $ Ar.second calcTf <$> Map.toList freq
  where
    calcTf :: Double -> Tf
    calcTf f = Tf $ 1 + f

getTf_IdfMaps :: [(Category, Map Term Tf)] -> [(Category, Map Term (Tf,Idf))]
getTf_IdfMaps set =
  map (Ar.second (getIdf set)) set

getIdf :: [(Category, Map Term Tf)] -> Map Term Tf -> Map Term (Tf, Idf)
getIdf set tfMap =
  Map.fromList $ map (\(term, tf) -> (term, (tf, calcIdf term))) $ Map.toList tfMap
  where
    calcIdf :: Term -> Idf
    calcIdf t =
      let count = length $ filter identity $ map (\(cat, termTf) -> Map.member t termTf) set in
      Idf $ log10 ((fromIntegral (length set) + 1) / fromIntegral (count + 1))

getTfIdfMaps :: [(Category, Map Term (Tf,Idf))] -> [(Category, Map Term Double)]
getTfIdfMaps xs =
  Ar.second combine <$> xs
  where
    combine :: Map Term (Tf,Idf) -> Map Term Double
    combine m =
      Map.fromList $ Ar.second combineTfIdf <$> Map.toList m


combineTfIdf :: (Tf, Idf) -> Double
combineTfIdf (Tf t, Idf f) = t * f

buildTfIdf :: TrainingSet -> TrainedData
buildTfIdf set =
  let ct = getCategoryTfs set in
  let mp = getTf_IdfMaps ct in
  TrainedData { catTf = ct
              , tf_Idf = mp
              , tfIdf = getTfIdfMaps mp
              , idf = Map.fromList $ Ar.second snd <$> concatMap (Map.toList . snd) mp
              }

scan :: TrainedData -> [Text] -> [(Category, Double)]
scan set terms =
  let searchTfIdfs = map (Ar.second combineTfIdf) $ Map.toList $ getIdf (catTf set) $ getTf terms in
  let compared = compareToCategory searchTfIdfs <$> tfIdf set in
  compared

  where
    sameCat :: (Term,Double) -> (Term,Double) -> Bool
    sameCat (at, av) (bt, bv) = at == bt

    compareToCategory :: [(Term, Double)] -> (Category, Map Term Double) -> (Category, Double)
    compareToCategory search (cat, tfMap) =
      let ts = Map.toList tfMap in
      let common = Lst.intersectBy sameCat ts search in
      let commonV = sum $ snd <$> common in
      let allV = sum (snd <$> ts) + sum  (snd <$> search) in

      (cat, (commonV * 2) / allV)

