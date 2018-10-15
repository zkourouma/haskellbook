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
  describe "Applicative a => List a" $ do
    it "has the EqProp" $
      quickBatch (applicative (Nil :: List ((String, String, Int))))
