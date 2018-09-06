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
