{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Classify
import ClassifyIO
import Prelude
import TfIdf
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "file name" $ do
    it "success if ends with txt" $ do
      isValidTxtName "aaa.txt" `shouldBe` True
    it "fails if does not end with txts" $ do
      isValidTxtName "aaa.tx" `shouldBe` False
  describe "getWords" $ do
    it "empty string" $ do
      [] `shouldBe` getWords "" 
    it "splits on space" $ do
     getWords "aaa bb1 cc2" `shouldBe` ["aaa", "bb1", "cc2"] 
    it "duplicates removed" $ do
     getWords "aaa bb1 aaa" `shouldBe`  ["aaa", "bb1"] 