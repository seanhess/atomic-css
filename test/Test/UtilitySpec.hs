module Test.UtilitySpec where

import Data.List (find)
import Skeletest
import Web.Atomic.CSS
import Web.Atomic.Types as Atomic


spec :: Spec
spec = do
  describe "display" $ do
    it "sets display:none, display:block" $ do
      let CSS rs = mempty ~ display None
      fmap (.properties) rs `shouldBe` [[Declaration "display" "none"]]

      let CSS rs2 = mempty ~ display Block
      fmap (.properties) rs2 `shouldBe` [[Declaration "display" "block"]]

  describe "TRBL" $ do
    it "sets all" $ do
      let CSS rs = mempty ~ pad 1
      mconcat (fmap (.properties) rs) `shouldBe` [Declaration "padding" "1px"]

    it "sets XY" $ do
      let CSS rs = mempty ~ pad (XY 1 0)
      let dcls = mconcat (fmap (.properties) rs)
      shouldHaveDeclaration "padding-top" "0px" dcls
      shouldHaveDeclaration "padding-left" "1px" dcls
      shouldHaveDeclaration "padding-bottom" "0px" dcls
      shouldHaveDeclaration "padding-right" "1px" dcls

    it "sets T R B L" $ do
      let CSS rs = mempty ~ pad (T 1) . pad (B 0) . pad (R 16) . pad (L 2)
      let dcls = mconcat (fmap (.properties) rs)
      shouldHaveDeclaration "padding-top" "1px" dcls
      shouldHaveDeclaration "padding-left" "0.125rem" dcls
      shouldHaveDeclaration "padding-bottom" "0px" dcls
      shouldHaveDeclaration "padding-right" "1.000rem" dcls

    it "sets X" $ do
      let CSS rs = mempty ~ pad (X 1)
      let dcls = mconcat (fmap (.properties) rs)
      shouldHaveDeclaration "padding-left" "1px" dcls
      shouldHaveDeclaration "padding-right" "1px" dcls

    it "sets TRBL" $ do
      let CSS rs = mempty ~ pad (TRBL 1 0 0 1)
      let dcls = mconcat (fmap (.properties) rs)
      shouldHaveDeclaration "padding-top" "1px" dcls
      shouldHaveDeclaration "padding-left" "1px" dcls
      shouldHaveDeclaration "padding-bottom" "0px" dcls
      shouldHaveDeclaration "padding-right" "0px" dcls


shouldHaveDeclaration :: Atomic.Property -> StyleValue -> [Declaration] -> IO ()
shouldHaveDeclaration p v ds = do
  let dcl = Declaration p v
  find (== dcl) ds `shouldBe` (Just dcl)
