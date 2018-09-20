module FunctorialSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           Functorial

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Identity" $ do
    it "has identity" $ quickCheck (functorIdentity :: IdId)
    it "is composable" $ quickCheck (functorCompose' :: IdComp)
  describe "Pair" $ do
    it "has identity" $ quickCheck (functorIdentity :: PairId)
    it "is composable" $ quickCheck (functorCompose' :: PairComp)
  describe "Two" $ do
    it "has identity" $ quickCheck (functorIdentity :: TwoId)
    it "is composable" $ quickCheck (functorCompose' :: TwoComp)
  describe "Three" $ do
    it "has identity" $ quickCheck (functorIdentity :: ThreeId)
    it "is composable" $ quickCheck (functorCompose' :: ThreeComp)
  describe "Three prime" $ do
    it "has identity" $ quickCheck (functorIdentity :: Three'Id)
    it "is composable" $ quickCheck (functorCompose' :: ThreePrimeComp)
  describe "Four" $ do
    it "has identity" $ quickCheck (functorIdentity :: FourId)
    it "is composable" $ quickCheck (functorCompose' :: FourComp)
  describe "Four prime" $ do
    it "has identity" $ quickCheck (functorIdentity :: Four'Id)
    it "is composable" $ quickCheck (functorCompose' :: FourPrimeComp)
