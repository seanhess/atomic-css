{-# LANGUAGE OverloadedLists #-}

module Web.Atomic.Render where

import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.String (IsString (..))
import Data.Text (Text, intercalate, pack)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import HTMLEntities.Text qualified as HE
import Web.Atomic.Html
import Web.Atomic.Types


renderLazyText :: Html () -> TL.Text
renderLazyText = TL.fromStrict . renderText


renderLazyByteString :: Html () -> BL.ByteString
renderLazyByteString = TLE.encodeUtf8 . renderLazyText


{- | Renders a 'View' as HTML with embedded CSS class definitions

>>> renderText $ el bold "Hello"
<style type='text/css'>.bold { font-weight:bold }</style>
<div class='bold'>Hello</div>
-}
renderText :: Html () -> Text
renderText html =
  let cs = cssRulesLines $ htmlCSSRules html
   in renderLines $ addCss cs $ htmlLines 2 html
 where
  addCss :: [Line] -> [Line] -> [Line]
  addCss [] cnt = cnt
  addCss cs cnt = do
    styleLines cs <> (Line Newline 0 "" : cnt)


htmlLines :: Int -> Html a -> [Line]
htmlLines ind (Html _ ns) = nodesLines ind ns


nodesLines :: Int -> [Node] -> [Line]
nodesLines ind ns = mconcat $ fmap (nodeLines ind) ns


nodeLines :: Int -> Node -> [Line]
nodeLines ind (Elem e) = elementLines ind e
nodeLines _ (Text t) = [Line Inline 0 $ HE.text t]
nodeLines _ (Raw t) = [Line Newline 0 t]


elementLines :: Int -> Element -> [Line]
elementLines ind elm =
  -- special rendering cases for the children
  case (elm.content :: [Node]) of
    [] ->
      -- auto closing creates a bug in chrome. An auto-closed div
      -- absorbs the next children
      [line $ open <> renderAttributes (elementAttributes elm) <> ">" <> close]
    [Text t] ->
      -- SINGLE text node, just display it indented
      [line $ open <> renderAttributes (elementAttributes elm) <> ">" <> HE.text t <> close]
    children ->
      -- normal indented rendering
      mconcat
        [ [line $ open <> renderAttributes (elementAttributes elm) <> ">"]
        , fmap (addIndent ind) $ nodesLines ind children
        , [line close]
        ]
 where
  open = "<" <> elm.name
  close = "</" <> elm.name <> ">"

  line t =
    if elm.inline
      then Line Inline 0 t
      else Line Newline 0 t


-- Attributes ---------------------------------------------------

-- | Element's attributes do not include class, which is separated. FlatAttributes generate the class attribute and include it
newtype FlatAttributes = FlatAttributes (Map Name AttValue)
  deriving newtype (Eq)


-- | The 'Web.View.Types.Attributes' for an element, inclusive of class.
elementAttributes :: Element -> FlatAttributes
elementAttributes e =
  FlatAttributes $
    addClasses (styleClass e) $
      e.attributes
 where
  addClasses :: AttValue -> Map Name AttValue -> Map Name AttValue
  addClasses "" as = as
  addClasses av as = M.insertWith (\a b -> a <> " " <> b) "class" av as

  styleClass :: Element -> AttValue
  styleClass elm =
    classesAttValue (elementClasses elm)


renderAttributes :: FlatAttributes -> Text
renderAttributes (FlatAttributes m) =
  case m of
    [] -> ""
    as -> " " <> T.unwords (map htmlAtt $ M.toList as)
 where
  htmlAtt (k, v) =
    k <> "=" <> "'" <> HE.text v <> "'"


-- REnder CSS --------------------------------------------

cssRulesLines :: Map Selector Rule -> [Line]
cssRulesLines = mapMaybe cssRuleLine . M.elems


cssRuleLine :: Rule -> Maybe Line
cssRuleLine r | null r.properties = Nothing
cssRuleLine r =
  let sel = (ruleSelector r).text
      props = intercalate "; " (map renderProp r.properties)
      med = mconcat $ fmap mediaCriteria $ r.media
   in Just $ Line Newline 0 $ wrapMedia med $ sel <> " { " <> props <> " }"
 where
  renderProp :: Declaration -> Text
  renderProp ((Property p) :. cv) = p <> ":" <> renderStyle cv

  renderStyle :: Style -> Text
  renderStyle (Style v) = pack v


wrapMedia :: MediaQuery -> Text -> Text
wrapMedia [] cnt = cnt
wrapMedia mqs cnt =
  "@media " <> mediaConditionsText mqs <> " { " <> cnt <> " }"
 where
  mediaConditionsText :: MediaQuery -> Text
  mediaConditionsText (MediaQuery cons) =
    T.intercalate " and " $ fmap (\c -> "(" <> c <> ")") cons


styleLines :: [Line] -> [Line]
styleLines [] = []
styleLines rulesLines =
  [Line Newline 0 "<style type='text/css'>"]
    <> rulesLines
    <> [Line Newline 0 "</style>"]


-- Lines ---------------------------------------
-- control inline vs newlines and indent

data Line = Line {end :: LineEnd, indent :: Int, text :: Text}
  deriving (Show, Eq)


instance IsString Line where
  fromString s = Line Newline 0 (pack s)


data LineEnd
  = Newline
  | Inline
  deriving (Eq, Show)


addIndent :: Int -> Line -> Line
addIndent n (Line e ind t) = Line e (ind + n) t


-- | Render lines to text
renderLines :: [Line] -> Text
renderLines = snd . L.foldl' nextLine (False, "")
 where
  nextLine :: (Bool, Text) -> Line -> (Bool, Text)
  nextLine (newline, t) l = (nextNewline l, t <> currentLine newline l)

  currentLine :: Bool -> Line -> Text
  currentLine newline l
    | newline = "\n" <> spaces l.indent <> l.text
    | otherwise = l.text

  nextNewline l = l.end == Newline

  spaces n = T.replicate n " "
