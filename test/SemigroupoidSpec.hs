module SemigroupoidSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           Semigroupoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Trivial" $
    it "is associative" $ quickCheck (semigroupAssoc :: TrivAssoc)
  describe "Identity" $
    it "is associative" $ quickCheck (semigroupAssoc :: IdentAssoc)
  describe "Two" $ it "is associative" $ quickCheck (semigroupAssoc :: TwoAssoc)
  describe "BoolConj" $
    it "is associative" $ quickCheck (semigroupAssoc :: BoolConjAssoc)
  describe "BoolDisj" $
    it "is associative" $ quickCheck (semigroupAssoc :: BoolDisjAssoc)
  describe "Or" $ it "is associative" $ quickCheck (semigroupAssoc :: OrAssoc)
  describe "BoolDisj" $
    it "is associative" $ quickCheck (semigroupAssoc :: BoolDisjAssoc)
