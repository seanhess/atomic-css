{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.RenderSpec (spec) where

import Control.Monad (zipWithM_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Skeletest
import Web.Atomic.CSS
import Web.Atomic.CSS.Select
import Web.Atomic.Html
import Web.Atomic.Render
import Web.Atomic.Types
import Web.Atomic.Types.Rule as Rule
import Prelude hiding (span)


spec :: Spec
spec = do
  describe "flatAttributes" flatSpec
  describe "lines" linesSpec
  describe "html" htmlSpec
  describe "css" $ do
    describe "media" mediaSpec
    describe "pseudo" pseudoSpec
    describe "rule" ruleSpec
  pure ()


mediaSpec :: Spec
mediaSpec = do
  it "wraps media" $ do
    wrapMedia (MediaQuery ["awesome", "another"]) "hello" `shouldBe` "@media (awesome) and (another) { hello }"

  it "converts to conditions" $ do
    mediaCriteria (MinWidth 100) `shouldBe` "min-width: 100px"

  it "renders media query" $ do
    cssRuleLine (addMedia (MinWidth 100) $ rule "bold" ["font-weight" :. "bold"]) `shouldBe` Just "@media (min-width: 100px) { .mmnw100\\:bold { font-weight:bold } }"


pseudoSpec :: Spec
pseudoSpec = do
  it "creates pseudo suffix" $ do
    let CSS rs = hover @(Html ()) bold $ CSS mempty
    fmap (ruleSelector) rs `shouldBe` [".hover\\:bold:hover"]


-- pseudoSuffix Hover `shouldBe` ":hover"
-- pseudoSuffix Even `shouldBe` ":nth-child(even)"
-- let r1 = rule "hello" [Declaration "key" "value"]
-- cssRuleLine r1 `shouldBe` Just ".hello { key:value }"

ruleSpec :: Spec
ruleSpec = do
  it "renders rules" $ do
    let r1 = rule "hello" ["key" :. "value"]
    cssRuleLine r1 `shouldBe` Just ".hello { key:value }"

    let r2 = rule "has2" ["k1" :. "val", "k2" :. "val"]
    cssRuleLine r2 `shouldBe` Just ".has2 { k1:val; k2:val }"

  it "no render empty rules" $ do
    cssRuleLine (Rule.fromClass "hello") `shouldBe` Nothing

  it "renders media" $ do
    let r = addMedia (MinWidth 100) $ rule "hello" ["key" :. "value"]
    ruleClassName r `shouldBe` "mmnw100:hello"
    ruleSelector r `shouldBe` ".mmnw100\\:hello"
    cssRuleLine r `shouldBe` Just "@media (min-width: 100px) { .mmnw100\\:hello { key:value } }"

  it "renders pseudo" $ do
    let r = addPseudo "hover" $ rule "hello" ["key" :. "value"]
    cssRuleLine r `shouldBe` Just ".hover\\:hello:hover { key:value }"

  it "renders pseudo + media" $ do
    let r = addMedia (MinWidth 100) $ addPseudo "hover" $ rule "hello" ["key" :. "value"]
    cssRuleLine r `shouldBe` Just "@media (min-width: 100px) { .mmnw100\\:hover\\:hello:hover { key:value } }"


-- let c = mediaCond (MaxWidth 800) bold
-- wrapMedia
-- Media (CSS [r]) <- pure c
-- r.selector `shouldBe` Selector ".mmxw800-bold"
-- r.className `shouldBe` ClassName "mmxw800-bold"
-- r.media `shouldBe` MediaQuery "(max-width: 800px)"

flatSpec :: Spec
flatSpec = do
  it "flattens empty" $ do
    let elm = element "div"
    elementAttributes elm `shouldBe` FlatAttributes []

  it "includes atts" $ do
    let elm = (element "div"){attributes = [("key", "value")]}
    elementAttributes elm `shouldBe` FlatAttributes [("key", "value")]

  it "includes classes in alphabetical order" $ do
    let elm = (element "div"){css = ["myclass", "another"]}
    elementAttributes elm `shouldBe` FlatAttributes [("class", "another myclass")]

  it "no duplicate attributes" $ do
    let Attributes attributes = att "key" "one" $ att "key" "two" $ mempty :: Attributes (Html ())
    let elm = (element "div"){attributes}
    elementAttributes elm `shouldBe` FlatAttributes [("key", "one")]

  it "no duplicate classes" $ do
    let elm = (element "div"){css = uniqueRules ["one", "one", "two"]}
    elementAttributes elm `shouldBe` FlatAttributes [("class", "one two")]

  it "classes are merged with css attribute" $ do
    let elm = (element "div"){css = ["mycss"], attributes = [("class", "default")]}
    elementAttributes elm `shouldBe` FlatAttributes [("class", "mycss default")]

  it "includes modified classnames" $ do
    let CSS rs = hover @(Html ()) bold $ CSS mempty
    let elm = (element "div"){css = rs}
    elementAttributes elm `shouldBe` FlatAttributes [("class", "hover:bold")]


linesSpec :: Spec
linesSpec = do
  it "adds indent" $ do
    addIndent 2 "hello" `shouldBe` Line Newline 2 "hello"

  it "renders basic" $ do
    renderLines ["hello"] `shouldBe` "hello"

  it "renders two" $ do
    renderLines ["<div>one</div>", "<div>two</div>"] `shouldBe` "<div>one</div>\n<div>two</div>"

  it "doesn't indent single line" $ do
    renderLines [Line Newline 2 "<div>one</div>"] `shouldNotBe` "  <div>one</div>"

  it "renders indent 2" $ do
    renderLines ["<div>", addIndent 2 "text", "</div>"] `shouldBe` "<div>\n  text\n</div>"

  it "renders inline" $ do
    renderLines [Line Inline 0 "one", Line Inline 0 "two"] `shouldBe` "onetwo"


htmlSpec :: Spec
htmlSpec = do
  describe "lines" $ do
    it "makes one line for single tag" $ do
      htmlLines 0 (tag "div" "hi") `shouldBe` [Line Newline 0 "<div>hi</div>"]

    it "makes two lines for double tags" $ do
      zipWithM_
        shouldBe
        (htmlLines 0 (tag "div" "hello" >> tag "div" "world"))
        [ Line Newline 0 "<div>hello</div>"
        , Line Newline 0 "<div>world</div>"
        ]

    it "indents contents" $ do
      zipWithM_
        shouldBe
        (htmlLines 2 (tag "div" $ tag "div" "one"))
        [ Line Newline 0 "<div>"
        , Line Newline 2 "<div>one</div>"
        , Line Newline 0 "</div>"
        ]

    it "inlines tags and text" $ do
      htmlLines 0 (text "one" >> text "two") `shouldBe` [Line Inline 0 "one", Line Inline 0 "two"]
      htmlLines 0 (inline "span" (text "hi") >> text "two") `shouldBe` [Line Inline 0 "<span>hi</span>", Line Inline 0 "two"]

    it "renders class" $ do
      htmlLines 0 (tag "div" ~ bold $ none) `shouldBe` ["<div class='bold'></div>"]

    it "renders pseudo class" $ do
      htmlLines 0 (tag "div" ~ hover bold $ none) `shouldBe` ["<div class='hover:bold'></div>"]

  describe "renderText" $ do
    it "renders simple output" $ do
      renderText (tag "div" "hi") `shouldBe` "<div>hi</div>"

    it "renders two elements" $ do
      renderText (tag "div" "hello" >> tag "div" "world") `shouldBe` "<div>hello</div>\n<div>world</div>"

    it "single-line with single text node" $ do
      renderText (tag "div" $ text "hello") `shouldBe` "<div>hello</div>"

    it "doesn't auto close tags " $ do
      renderText (tag "div" none) `shouldBe` "<div></div>"

    it "renders inline" $ do
      renderText (inline "span" "hello" >> text "woot" >> inline "span" "world") `shouldBe` "<span>hello</span>woot<span>world</span>"

    it "renders ?" $ do
      renderText (tag "div" $ text "txt" >> tag "div" none >> text "txt") `shouldBe` "<div>\n  txt<div></div>\n  txt</div>"

    it "matches basic output with styles" $ do
      basic <- T.readFile "test/resources/basic.txt"
      let html = do
            row ~ pad 10 $ do
              el ~ bold $ "hello"
              el "world"
      let out = renderText html
      zipWithM_ shouldBe (T.lines out) (T.lines basic)

    it "renders external classes" $ do
      renderText (el ~ cls "woot" $ none) `shouldBe` "<div class='woot'></div>"

  -- it "matches tooltips big example" $ do
  --   golden <- T.readFile "test/resources/tooltips.txt"
  --   let out = renderText tooltips
  --   putStrLn $ unpack out
  --   zipWithM_ shouldBe (T.lines out) (T.lines golden)

  describe "escape" $ do
    it "should escape bad attributes" $ do
      renderText (tag "div" @ att "title" "bob's" $ none) `shouldBe` "<div title='bob&#39;s'></div>"
      renderText (tag "div" @ att "title" "bob\"s" $ none) `shouldBe` "<div title='bob&quot;s'></div>"
      renderText (tag "div" @ att "title" "1<2" $ none) `shouldBe` "<div title='1&lt;2'></div>"

    it "should escape bad text" $ do
      renderText (text "<script>bad</script>") `shouldBe` "&lt;script&gt;bad&lt;/script&gt;"

    it "should not escape raw" $ do
      renderText (raw "<script>bad</script>") `shouldBe` "<script>bad</script>"
      renderText (raw "bob's \"buddy\"") `shouldBe` "bob's \"buddy\""

  describe "classes" $ do
    it "should add utility classes" $ do
      htmlLines 0 (tag "div" ~ bold . pad 10 $ none) `shouldBe` ["<div class='bold p-10'></div>"]

    it "should override in composition order" $ do
      htmlLines 0 (tag "div" ~ pad 10 . pad 5 $ none) `shouldBe` ["<div class='p-10'></div>"]

    it "should override in styleable order" $ do
      htmlLines 0 (tag "div" ~ pad 10 ~ pad 5 $ none) `shouldBe` ["<div class='p-5'></div>"]

    it "merges class attribute if set" $ do
      htmlLines 0 (tag "div" @ att "class" "hello" ~ bold . pad 5 $ none) `shouldBe` ["<div class='bold p-5 hello'></div>"]
 where
  inline :: Text -> Html () -> Html ()
  inline nm (Html _ content) = do
    Html () [Elem $ Element True nm mempty mempty content]


-- tooltips :: Html ()
-- tooltips = do
--   let items :: [Text] = ["One", "Two", "Three", "Four", "Five", "Six"]
--   col ~ pad 10 . gap 10 . width 300 $ do
--     el ~ bold $ "CSS ONLY TOOLTIPS"
--     el "some stuff"
--     text "sometext"
--     mapM_ tooltipItem items
--
-- tooltipItem :: Text -> Html ()
-- tooltipItem item = do
--   el ~ stack . showTooltips . hover (color red) $ do
--     el ~ border 1 . bg white $ text item
--     el ~ cls "tooltip" . popup (TR 10 10) . zIndex 1 . hidden $ do
--       col ~ border 2 . gap 5 . bg white . pad 5 $ do
--         el ~ bold $ "ITEM DETAILS"
--         el $ text item
--
-- showTooltips =
--   css
--     "tooltips"
--     ".tooltips:hover > .tooltip"
--     [Declaration "visibility" "visible"]
--
-- red = HexColor "#F00"
-- white = HexColor "#FFF"

-- col :: Html () -> Html ()
-- col = el ~ flexRow

row :: Html () -> Html ()
row = el ~ flexCol


el :: Html () -> Html ()
el = tag "div"

-- it "psuedo + parent" $ do
--   let sel = (selector "myclass"){ancestor = Just "parent", pseudo = Just Hover}
--   selectorText sel `shouldBe` ".parent .hover\\:parent-myclass:hover"
--
-- it "child" $ do
--   let sel = (selector "myclass"){child = Just "mychild"}
--   attributeClassName sel `shouldBe` "myclass-mychild"
--   selectorText sel `shouldBe` ".myclass-mychild > .mychild"
--
--   let sel2 = (selector "myclass"){child = Just AllChildren}
--   attributeClassName sel2 `shouldBe` "myclass-all"
--   selectorText sel2 `shouldBe` ".myclass-all > *"
--
-- it "parent + pseudo + child" $ do
--   let sel = (selector "myclass"){child = Just "mychild", ancestor = Just "myparent", pseudo = Just Hover}
--   attributeClassName sel `shouldBe` "hover:myparent-myclass-mychild"
--   selectorText sel `shouldBe` ".myparent .hover\\:myparent-myclass-mychild:hover > .mychild"

-- describe "child combinator" $ do
--   it "should include child combinator in definition"  $ do
