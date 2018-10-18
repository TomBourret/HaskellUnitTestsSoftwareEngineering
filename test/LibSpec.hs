module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do

  describe "Lca function" $ do

    it "returns empty for empty path in first" $ do
      (lca empty path1) `shouldBe` empty

    it "returns empty for empty path in second" $ do
      (lca path1 empty) `shouldBe` empty

    it "returns [1] for lca path1 path2" $ do
      (lca path1 path2) `shouldBe` pathRes1

    it "returns [3,1] for lca path2 path3" $ do
      (lca path2 path3) `shouldBe` pathRes2

    it "returns path1 for lca path1 path1" $ do
      (lca path1 path1) `shouldBe` path1

    it "returns 5 for path_length path1" $ do
      (path_length path1) `shouldBe` 5

    it "returns 4 for path_length path2" $ do
      (path_length path2) `shouldBe` 4

    it "returns 0 for path_length empty" $ do
      (path_length empty) `shouldBe` 0