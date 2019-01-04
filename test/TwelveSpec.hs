module TwelveSpec
  ( main
  , spec
  ) where

import           Test.Hspec

import           Twelve

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "replaceThe" $
    it "swaps 'the' with 'a'" $
    replaceThe "the cow loves us" `shouldBe` "a cow loves us"
  describe "countTheBeforeVowel" $
    it "counts the 'the's before vowels" $ do
      countTheBeforeVowel "the cow" `shouldBe` 0
      countTheBeforeVowel "the evil cow" `shouldBe` 1
  describe "countVowels" $
    it "counts the vowels" $ do
      countVowels "the cow" `shouldBe` 2
      countVowels "Mikolajczak" `shouldBe` 4
  describe "natToInteger" $ do
    it "knows Zero" $ natToInteger Zero `shouldBe` 0
    it "knows Succ" $ do
      natToInteger (Succ Zero) `shouldBe` 1
      natToInteger (Succ (Succ Zero)) `shouldBe` 2
  describe "integerToNat" $ do
    it "knows Zero" $ integerToNat 0 `shouldBe` Just Zero
    it "knows Succ" $ do
      integerToNat 1 `shouldBe` Just (Succ Zero)
      integerToNat 2 `shouldBe` Just (Succ (Succ Zero))
    it "throws out negative numbers" $ integerToNat (-1) `shouldBe` Nothing
