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
spec =
  describe "Trivial" $
  it "is associative" $ quickCheck (semigroupAssoc :: TrivAssoc)
