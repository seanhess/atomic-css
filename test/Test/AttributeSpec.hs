module Test.AttributeSpec where

import Data.Map.Strict qualified as M
import Skeletest
import Web.Atomic.CSS
import Web.Atomic.Html
import Web.Atomic.Types


spec :: Spec
spec = do
  describe "Attributable" $ do
    it "applies attributes" $ do
      let Attributes m = mempty @ att "key" "value" . att "one" "one"
      M.keys m `shouldBe` ["key", "one"]

    it "overrides in composition order" $ do
      let Attributes m = mempty @ att "key" "two" . att "key" "one"
      M.toList m `shouldBe` [("key", "two")]

    it "overrides in operator order" $ do
      let Attributes m = mempty @ att "key" "two" @ att "key" "one"
      M.toList m `shouldBe` [("key", "one")]

    it "operator precedence works both ways" $ do
      let _ = tag "div" @ att "one" "value" $ "contents"
      let _ = tag "div" ~ bold @ att "one" "value" $ "contents"
      pure ()

    -- IF statements must have parentheses :/
    it "operator precedence works with if statements" $ do
      let _ =
            tag "div"
              @ att "one" "value"
              . ( if True
                    then att "two" "value"
                    else id
                )
              $ text "contents"
      pure ()
