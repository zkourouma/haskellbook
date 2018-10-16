module ApplicativesSpec
  ( main
  , spec
  ) where

import           Data.Monoid
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Applicatives

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "List a" $
    it "satisfies the applicative laws" $
    quickBatch (applicative (Nil :: List (String, String, Int)))
  describe "ZipList' a" $
    it "satisfies the applicative laws" $
    quickBatch (applicative (ZipList' Nil :: ZipList' (String, String, Int)))
