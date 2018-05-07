module ChNineSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           ChNine

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "zzip" $ do
    it "handles empty lists" $ zzip [1, 2, 3] ([] :: [Integer]) `shouldBe` []
    it "combines lists into tuples" $
      zzip [1, 2, 3, 4] [4, 5, 6] `shouldBe` [(1, 4), (2, 5), (3, 6)]
    -- it "behaves like the normal zip" $
    --   property $ \l l' -> (zzip l l') === (zip l l')
  describe "zzipWith" $ do
    it "handles empty lists" $
      zzipWith (\a b -> (a, b)) [1, 2, 3] ([] :: [Double]) `shouldBe` []
    it "combines lists into sums" $
      zzipWith (+) [1, 2, 3] [4, 5, 6, 7] `shouldBe` [5, 7, 9]
    -- it "behaves like the normal zipWith" $
    --   property $ \fn l l' -> zzipWith fn l l' === zipWith fn l l'
