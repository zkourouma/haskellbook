module ChElevenSpec
  ( main
  , spec
  ) where

import           Test.Hspec

import           ChEleven

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mapTree" $
    it "takes a function and applies it to a tree" $
    mapTree (+ 1) (Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)) `shouldBe`
    Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
  describe "preorder" $
    it "returns a list in pre order" $
    preorder (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) `shouldBe` [2, 1, 3]
  describe "inorder" $
    it "returns a list in order" $
    inorder (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) `shouldBe` [1, 2, 3]
  describe "postorder" $
    it "returns a list in post order" $
    postorder (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) `shouldBe`
    [1, 3, 2]
  describe "foldTree" $
    it "implements foldr for BinaryTree" $ do
      foldTree (+) 4 (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) `shouldBe`
        10
      foldTree
        (++)
        ""
        (Node (Node Leaf "baz" Leaf) "foo" (Node Leaf "bar" Leaf)) `shouldBe`
        "bazfoobar"
