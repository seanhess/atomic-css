module Test.StyleSpec (spec) where

import Skeletest
import Web.Atomic.CSS
import Web.Atomic.Types
import Prelude hiding (span)


spec :: Spec
spec = do
  mainSpec
  selectorSpec


mainSpec :: Spec
mainSpec = do
  describe "PropertyStyle" $ do
    it "should compile, and set both the className and styles" $ do
      let rs = runCSS @[Rule] $ list Decimal
      length rs `shouldBe` 1
      [c] <- pure rs
      ruleClassName c `shouldBe` ClassName "list-decimal"
      ruleSelector c `shouldBe` ".list-decimal"
      c.properties `shouldBe` [Declaration "list-style-type" "decimal"]

    it "should work with outside member None" $ do
      let rs = runCSS @[Rule] $ list None
      length rs `shouldBe` 1
      [c] <- pure rs
      ruleClassName c `shouldBe` ClassName "list-none"
      ruleSelector c `shouldBe` ".list-none"
      c.properties `shouldBe` [Declaration "list-style-type" "none"]

  describe "PxRem" $ do
    it "uses absolutes for 0,1" $ do
      toStyleValue (PxRem 0) `shouldBe` "0px"
      toStyleValue (PxRem 16) `shouldBe` "1.000rem"

    it "uses rem for others" $ do
      toStyleValue (PxRem 2) `shouldBe` "0.125rem"
      toStyleValue (PxRem 10) `shouldBe` "0.625rem"
      toStyleValue (PxRem 16) `shouldBe` "1.000rem"

  describe "Length" $ do
    it "styles pct" $ do
      toStyleValue (Pct (1 / 3)) `shouldBe` "33.3%"

    it "adds values" $ do
      toStyleValue (PxRem 6 + PxRem 10) `shouldBe` "1.000rem"

  describe "Align" $ do
    it "should produce correct style values" $ do
      toStyleValue AlignCenter `shouldBe` "center"
      toStyleValue AlignJustify `shouldBe` "justify"

  describe "ToClassName" $ do
    it "should hyphenate classnames" $ do
      "woot" -. None `shouldBe` "woot-none"

    it "should not hyphenate with empty suffix" $ do
      "woot" -. () `shouldBe` "woot"

    it "should escape classNames" $ do
      className "hello.woot-hi" `shouldBe` ClassName "hello-woot-hi"

  describe "Colors" $ do
    it "correct styleValue independent of leading slash" $ do
      toStyleValue (HexColor "#FFF") `shouldBe` StyleValue "#FFF"
      toStyleValue (HexColor "FFF") `shouldBe` StyleValue "#FFF"
      toStyleValue ("FFF" :: HexColor) `shouldBe` StyleValue "#FFF"
      toStyleValue ("#FFF" :: HexColor) `shouldBe` StyleValue "#FFF"

    it "correct className independent of leading slash" $ do
      toClassName (HexColor "#FFF") `shouldBe` "fff"
      toClassName (HexColor "FFF") `shouldBe` "fff"
      toClassName ("FFF" :: HexColor) `shouldBe` "fff"
      toClassName ("#FFF" :: HexColor) `shouldBe` "fff"

    it "works with custom colors" $ do
      toStyleValue (colorValue Danger) `shouldBe` StyleValue "#F00"
      toStyleValue (colorValue Warning) `shouldBe` StyleValue "#FF0"

  describe "Styleable" $ do
    it "applies styles" $ do
      let rs :: [Rule] = [] ~ bold . fontSize 24
      fmap (.className) rs `shouldBe` ["bold", "fs-24"]

    it "writes in composition order" $ do
      let rs :: [Rule] = [] ~ bold . fontSize 12 . italic
      fmap (.className) rs `shouldBe` ["bold", "fs-12", "italic"]

    it "overrides in operator order" $ do
      let rs :: [Rule] = [] ~ bold . fontSize 12 ~ italic
      fmap (.className) rs `shouldBe` ["italic", "bold", "fs-12"]

  describe "External Classes" $ do
    it "adds external classes" $ do
      let rs :: [Rule] = [] ~ cls "external"
      rs `shouldBe` [Rule "external" mempty mempty []]
      fmap (.className) rs `shouldBe` ["external"]


selectorSpec :: Spec
selectorSpec = do
  describe "Selector" $ do
    it "normal selector" $ do
      selector "myclass" `shouldBe` Selector ".myclass"

    it "escapes colons" $ do
      selector "hover:bold" `shouldBe` Selector ".hover\\:bold"


data AppColor
  = Danger
  | Warning
  deriving (Show, Eq)


instance ToColor AppColor where
  colorValue Danger = "#F00"
  colorValue Warning = "FF0"
