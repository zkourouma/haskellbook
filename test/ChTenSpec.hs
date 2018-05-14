module ChTenSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           ChTen

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "myOr" $ it "acts like or" $ myOr [True, False, True] `shouldBe` True
  describe "myAny" $
    it "acts like any" $ do
      myAny even [1, 3, 5] `shouldBe` False
      myAny odd [1, 3, 5] `shouldBe` True
  describe "myElem" $
    it "acts like elem" $ do
      myElem 1 [1 .. 10] `shouldBe` True
      myElem 1 [2 .. 10] `shouldBe` False
  describe "myReverse" $
    it "acts like reverse" $ do
      myReverse "blah" `shouldBe` "halb"
      myReverse [1 .. 5] `shouldBe` [5, 4, 3, 2, 1]
  describe "myMap" $
    it "acts like map" $ do
      myMap even [1 .. 5] `shouldBe` [False, True, False, True, False]
      myMap (+ 1) [1 .. 5] `shouldBe` [2 .. 6]
  describe "myFilter" $
    it "acts like filter" $ do
      myFilter even [1 .. 5] `shouldBe` [2, 4]
      myFilter odd [1 .. 5] `shouldBe` [1, 3, 5]
  describe "squish" $
    it "acts like concat" $ do
      squish [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe` [1 .. 9]
  describe "squishMap" $
    it "maps a function over a list and concatenates the results" $ do
      squishMap (\x -> [1, x, 3]) [2] `shouldBe` [1, 2, 3]
      squishMap (\x -> "WO " ++ [x] ++ " HOO ") "123" `shouldBe`
        "WO 1 HOO WO 2 HOO WO 3 HOO "
  describe "squishAgain" $
    it "flattens a list of lists into a list using squishMap" $
    squishAgain [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe`
    [1, 2, 3, 4, 5, 6, 7, 8, 9]
  describe "myMaximumBy" $
    it "takes a comp function and a list and returns the greatest element" $
    myMaximumBy compare [1, 53, 9001, 10] `shouldBe` 9001
  describe "myMinimumBy" $
    it "takes a comp function and a list and returns the least element" $
    myMinimumBy compare [1, 53, 9001, 10] `shouldBe` 1
