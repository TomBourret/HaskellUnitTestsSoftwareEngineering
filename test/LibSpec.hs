module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do

  describe "Lca function" $ do

    it "returns Left False for empty tree" $ do
      (lca emptyTree 1 2) `shouldBe` Left False