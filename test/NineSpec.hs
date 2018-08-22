module NineSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           Nine

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "zzip" $ do
    it "handles empty lists" $ zzip [1, 2, 3] ([] :: [Integer]) `shouldBe` []
    it "combines lists into tuples" $
      zzip [1, 2, 3, 4] [4, 5, 6] `shouldBe` [(1, 4), (2, 5), (3, 6)]
  describe "zzipWith" $ do
    it "handles empty lists" $
      zzipWith (\a b -> (a, b)) [1, 2, 3] ([] :: [Double]) `shouldBe` []
    it "combines lists into sums" $
      zzipWith (+) [1, 2, 3] [4, 5, 6, 7] `shouldBe` [5, 7, 9]
  describe "yell" $
    it "recursively uppercases a string" $ yell "woot" `shouldBe` "WOOT"
  describe "yelling" $
    it "shows only capital letters" $ yelling "HbEfLrLxO" `shouldBe` "HELLO"
  describe "titleize" $
    it "capitalizes the first letter of a string" $
    titleize "julie" `shouldBe` "Julie"
  describe "myOr" $
    it "recursively returns True if any bool in its list is True" $ do
      myOr [False, True, False] `shouldBe` True
      myOr [False, False, False] `shouldBe` False
  describe "myAny" $
    it
      "returns True if a -> Bool applied to of of the values in the list returns True" $ do
      myAny odd [1, 3, 5] `shouldBe` True
      myAny even [1, 3, 5] `shouldBe` False
  describe "myElem" $
    it "uses any to determine if an input is a member of the list" $ do
      myElem 1 [1 .. 10] `shouldBe` True
      myElem 1 [2 .. 10] `shouldBe` False
  describe "myReverse" $
    it "recursively reverses strings" $ do
      myReverse "blah" `shouldBe` "halb"
      myReverse [1 .. 5] `shouldBe` [5, 4, 3, 2, 1]
  describe "squish" $
    it "flattens a list of lists into a list" $
    squish [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe`
    [1, 2, 3, 4, 5, 6, 7, 8, 9]
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
  describe "myMaximum" $
    it "should replicate maximum" $
    myMaximum [1, 53, 9001, 10] `shouldBe` maximum [1, 53, 9001, 10]
  describe "myMinimum" $
    it "should replicate minimum" $
    myMinimum [1, 53, 9001, 10] `shouldBe` minimum [1, 53, 9001, 10]
