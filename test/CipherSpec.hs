module CipherSpec
  ( main
  , spec
  ) where

import           Cipher
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "caesar" $ do
    it "moves" $ caesar 1 "abcd" `shouldBe` "bcde"
    it "wraps forwards" $ caesar 1 "wxyz" `shouldBe` "xyza"
    it "wraps backwards" $ caesar (-1) "abcd" `shouldBe` "zabc"
  describe "unCaesar" $ do
    it "moves" $ unCaesar 1 "bcde" `shouldBe` "abcd"
    it "wraps forwards" $ unCaesar 1 "xyza" `shouldBe` "wxyz"
    it "wraps backwards" $ unCaesar (-1) "zabc" `shouldBe` "abcd"
