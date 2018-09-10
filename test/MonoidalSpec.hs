module MonoidalSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           Monoidal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "First'" $ do
    it "is has associativity" $ quickCheck (monoidAssoc :: FirstMappend)
    it "is has left identity" $ quickCheck (monoidLeftIdentity :: FstId)
    it "is has right identity" $ quickCheck (monoidRightIdentity :: FstId)
  describe "Identity" $ do
    it "is has associativity" $ quickCheck (monoidAssoc :: IdenMappend)
    it "is has left identity" $ quickCheck (monoidLeftIdentity :: IdenId)
    it "is has right identity" $ quickCheck (monoidRightIdentity :: IdenId)
  describe "Two" $ do
    it "is has associativity" $ quickCheck (monoidAssoc :: TwoMappend)
    it "is has left identity" $ quickCheck (monoidLeftIdentity :: TwoId)
    it "is has right identity" $ quickCheck (monoidRightIdentity :: TwoId)
  describe "BoolConj" $ do
    it "is has associativity" $ quickCheck (monoidAssoc :: BoolConjMappend)
    it "is has left identity" $ quickCheck (monoidLeftIdentity :: BoolConjId)
    it "is has right identity" $ quickCheck (monoidRightIdentity :: BoolConjId)
  describe "BoolDisj" $ do
    it "is has associativity" $ quickCheck (monoidAssoc :: BoolDisjMappend)
    it "is has left identity" $ quickCheck (monoidLeftIdentity :: BoolDisjId)
    it "is has right identity" $ quickCheck (monoidRightIdentity :: BoolDisjId)
