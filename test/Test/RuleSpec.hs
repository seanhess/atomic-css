module Test.RuleSpec where

import Skeletest
import Web.Atomic.CSS.Select (addAncestor, addMedia, addPseudo)
import Web.Atomic.Types
import Web.Atomic.Types.Rule as Rule


spec :: Spec
spec = do
  describe "Unique Rules" $ do
    it "should only set same class once" $ do
      uniqueRules ["asdf", "asdf"] `shouldBe` ["asdf"]

      fmap (.className) [bold, bold] `shouldBe` ["bold", "bold"]
      fmap (.className) (uniqueRules [bold, bold]) `shouldBe` ["bold"]

    it "should set different properties" $ do
      let rs = [bold, fs12]
      length (uniqueRules rs) `shouldBe` 2

    it "should unset same property" $ do
      let rs = [fs24, bold, fs12]
      fmap (.className) (uniqueRules rs) `shouldBe` ["fs-24", "bold"]

    -- it "should unset same property using (~)" $ do
    --   let rs = [] ~ fontSize 12 . bold ~ fontSize 24
    --   length rs `shouldBe` 3
    --   fmap (.className) (uniqueRules rs) `shouldBe` ["fs-24", "bold"]

    it "should treat hover states as unique" $ do
      let hoverBold = addPseudo "hover" bold
          hoverNormal = addPseudo "hover" normal
          hoverActiveNormal = addPseudo "hover" $ addPseudo "active" normal

      length (uniqueRules [hoverBold, normal]) `shouldBe` 2
      length (uniqueRules [hoverBold, hoverNormal]) `shouldBe` 1
      length (uniqueRules [hoverActiveNormal, hoverBold]) `shouldBe` 2

    it "should ignore custom selectors" $ do
      length (uniqueRules [bold, custom]) `shouldBe` 2
      length (uniqueRules [custom, bold]) `shouldBe` 2

  describe "className" $ do
    it "basic" $ do
      ruleClassName (Rule.fromClass "hello") `shouldBe` "hello"

    it "includes pseudo" $ do
      ruleClassName (addPseudo "active" $ addPseudo "hover" $ "hello") `shouldBe` "active:hover:hello"

    it "includes media" $ do
      ruleClassName (addMedia (MinWidth 100) "hello") `shouldBe` "mmnw100:hello"

    it "includes pseudo + media" $ do
      ruleClassName (addMedia (MinWidth 100) $ addPseudo "hover" "hello") `shouldBe` "mmnw100:hover:hello"

  -- it "doesn't change with custom selectors" $ do
  --   ruleClassName (Rule "hello" (Just ".hello") [Hover] [MinWidth 100] []) `shouldBe` "hello"

  describe "selector" $ do
    it "creates selector from class name" $ do
      ruleSelector (Rule.fromClass "p-10") `shouldBe` ".p-10"

    it "adds pseudo" $ do
      ruleSelector (addPseudo "hover" "p-10") `shouldBe` ".hover\\:p-10:hover"

    it "adds media" $ do
      ruleSelector (addMedia (MinWidth 100) "hello") `shouldBe` ".mmnw100\\:hello"

    it "adds pseudo + media " $ do
      ruleSelector (addMedia (MinWidth 100) $ addPseudo "hover" "hello") `shouldBe` ".mmnw100\\:hover\\:hello:hover"

  describe "ancestor" $ do
    it "prepends selector" $ do
      let r = addAncestor "htmx-request" "hello"
      let cn = ruleClassName r
      cn `shouldBe` "htmx-request:hello"
      ruleSelector r `shouldBe` ".htmx-request " <> selector cn

    it "ancestor + pseudo" $ do
      let r = addAncestor "htmx-request" $ addPseudo "hover" "hello"
      let cn = ruleClassName r
      cn `shouldBe` "htmx-request:hover:hello"
      ruleSelector r `shouldBe` ".htmx-request " <> selector cn <> ":hover"

    -- what dopes this mean? Are they the same?
    -- hover (ancestor "htmx-request" "bold")
    -- ancestor "htmx-request" (hover "bold")
    -- certain things should be outermost....
    it "pseudo + ancestor" $ do
      let r = addPseudo "hover" $ addAncestor "htmx-request" "hello"
      let cn = ruleClassName r
      cn `shouldBe` "hover:htmx-request:hello"
      ruleSelector r `shouldBe` ".htmx-request " <> selector cn <> ":hover"

    it "ignores when custom selector" $ do
      let r = addAncestor "htmx-request" $ addPseudo "hover" $ (rule "hello" []){selector = CustomRule ".woot"}
      let cn = ruleClassName r
      cn `shouldBe` "hello"
      ruleSelector r `shouldBe` ".woot"
 where
  -- it "doesn't change with custom selectors" $ do
  --   ruleSelector (Rule "hello" (Just ".hello") [Hover] [MinWidth 100] []) `shouldBe` ".hello"

  fs12 = Rule "fs-12" mempty mempty ["font-size" :. "12px"]
  fs24 = Rule "fs-24" mempty mempty ["font-size" :. "24px"]
  bold = Rule "bold" mempty mempty ["font-weight" :. "bold"]
  normal = Rule "normal" mempty mempty ["font-weight" :. "normal"]
  custom = Rule "custom" (CustomRule ".custom > *") mempty ["font-weight" :. "bold"]
