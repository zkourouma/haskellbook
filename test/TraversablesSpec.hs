module TraversablesSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Traversables

type Intz = (Int, Int, [Int])

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Identity" $
    it "is traversable" $
    quickBatch (traversable (Identity (0, 0, []) :: Identity Intz))
  describe "Constant" $
    it "is traversable" $
    quickBatch (traversable (Constant (0, 0, []) :: Constant Intz Intz))
  describe "Optional" $
    it "is traversable" $
    quickBatch (traversable (Yep (0, 0, []) :: Optional Intz))
  describe "List" $
    it "is traversable" $ quickBatch (traversable (Nil :: List Intz))
  describe "Three" $
    it "is traversable" $
    quickBatch (traversable (Three 0 0 (0, 0, []) :: Three Int Int Intz))
  describe "Big" $
    it "is traversable" $
    quickBatch (traversable (Big 0 (0, 0, []) (0, 0, []) :: Big Int Intz))
  describe "Tree" $
    it "is traversable" $ quickBatch (traversable (Empty :: Tree Intz))
