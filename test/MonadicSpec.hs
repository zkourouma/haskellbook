module MonadicSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Monadic

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Nope" $
    it "is a monad" $ quickBatch (monad (NopeDotJpg :: Nope (Int, String, Int)))
  describe "PhhhbbtttEither" $
    it "is a monad" $
    quickBatch
      (monad
         (Monadic.Left (0, "", 0) :: PhhhbbtttEither (Int, String, Int) ( Int
                                                                        , String
                                                                        , Int)))
  describe "Identity" $
    it "is a monad" $
    quickBatch (monad (Identity (0, "", 0) :: Identity (Int, String, Int)))
  describe "List" $
    it "is a monad" $ quickBatch (monad (Nil :: List (Int, String, Int)))
