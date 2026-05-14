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
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Encoding qualified as TLE
import HTMLEntities.Text qualified as HE
import Web.Atomic.Html
import Web.Atomic.Types


renderLazyText :: Html () -> TL.Text
renderLazyText = TB.toLazyText . renderBuilder


renderLazyByteString :: Html () -> BL.ByteString
renderLazyByteString = TLE.encodeUtf8 . renderLazyText


renderText :: Html () -> Text
renderText = TL.toStrict . renderLazyText


renderBuilder :: Html () -> Builder
renderBuilder html =
  let cs = cssRulesLines $ htmlCSSRules html
   in renderLines $ addCss cs $ htmlLines 0 html
 where
  addCss :: [Line] -> [Line] -> [Line]
  addCss [] cnt = cnt
  addCss cs cnt = do
    styleLines cs <> (Line Newline 0 "" : cnt)


htmlLines :: Int -> Html a -> [Line]
htmlLines indent (Html _ ns) = nodesLines indent ns


nodesLines :: Int -> [Node] -> [Line]
nodesLines indent = concatMap (nodeLines indent)


nodeLines :: Int -> Node -> [Line]
nodeLines indent (Elem e) = elementLines indent e
nodeLines indent (Text t) = [Line Inline indent $ TB.fromText (HE.text t)]
nodeLines indent (Raw t) = [Line Inline indent $ TB.fromText t]


elementLines :: Int -> Element -> [Line]
elementLines indent elm =
  -- special rendering cases for the children
  case (elm.content :: [Node]) of
    [] ->
      -- auto closing creates a bug in chrome. An auto-closed div
      -- absorbs the next children
      [line $ open <> attrs <> ">" <> close]
    [Text t] ->
      -- SINGLE text node, just display it indented
      [line $ open <> attrs <> ">" <> HE.text t <> close]
    children ->
      -- normal indented rendering
      mconcat
        [ [line $ open <> attrs <> ">"]
        , nodesLines (indent + 2) children
        , [line close]
        ]
 where
  attrs = renderAttributes $ elementAttributes elm
  open = "<" <> elm.name
  close = "</" <> elm.name <> ">"

  line t = Line (if elm.inline then Inline else Newline) indent (TB.fromText t)


-- Attributes ---------------------------------------------------

-- | Element's attributes do not include class, which is separated. FlatAttributes generate the class attribute and include it
newtype FlatAttributes = FlatAttributes (Map Name AttValue)
  deriving newtype (Eq)


-- | The 'FlatAttributes' for an element, inclusive of class.
elementAttributes :: Element -> FlatAttributes
elementAttributes e =
  FlatAttributes $
    addClasses
      (styleClass e)
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
      med = mconcat $ fmap mediaCriteria r.media
   in Just $ Line Newline 0 $ TB.fromText $ wrapMedia med $ sel <> " { " <> props <> " }"
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

data Line = Line {end :: LineEnd, indent :: Int, text :: Builder}
  deriving (Show, Eq)


instance IsString Line where
  fromString s = Line Newline 0 (TB.fromString s)


data LineEnd
  = Newline
  | Inline
  deriving (Eq, Show)


-- | Render lines to text
renderLines :: [Line] -> Builder
renderLines = mconcat . snd . L.mapAccumL nextLine Inline
 where
  nextLine :: LineEnd -> Line -> (LineEnd, Builder)
  nextLine end l = (l.end, renderLine end l)

  renderLine :: LineEnd -> Line -> Builder
  renderLine end l
    | end == Newline = "\n" <> spaces l.indent <> l.text
    | otherwise = l.text

  spaces n = TB.fromText $ T.replicate n " "
