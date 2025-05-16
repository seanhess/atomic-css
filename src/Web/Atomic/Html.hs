{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Web.Atomic.Html where

import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import GHC.Exts (IsList (..))
import Web.Atomic.Types


-- | A single HTML tag. Note that the class attribute is generated separately from the css, rather than the attributes
data Element = Element
  { inline :: Bool
  , name :: Text
  , css :: [Rule]
  , attributes :: Map Name AttValue
  , content :: [Node]
  }


data Html a = Html {value :: a, nodes :: [Node]}


instance IsList (Html ()) where
  type Item (Html ()) = Node
  fromList = Html () . fromList
  toList (Html _ ns) = ns


instance IsString (Html ()) where
  fromString s = Html () [fromString s]


instance Functor Html where
  fmap f (Html a ns) = Html (f a) ns


instance Applicative Html where
  pure a = Html a []
  (<*>) :: Html (a -> b) -> Html a -> Html b
  Html f nfs <*> Html a nas =
    Html (f a) (nfs <> nas)


-- ha *> hb = ha <> hb
instance Monad Html where
  (>>=) :: forall a b. Html a -> (a -> Html b) -> Html b
  Html a nas >>= famb =
    let Html b nbs = famb a :: Html b
     in Html b (nas <> nbs)


data Node
  = Elem Element
  | Text Text
  | Raw Text


instance IsString Node where
  fromString s = Text (pack s)


mapElement :: (Element -> Element) -> Html a -> Html a
mapElement f (Html a ns) = Html a $ fmap (mapNodeElement f) ns


mapNodeElement :: (Element -> Element) -> Node -> Node
mapNodeElement f (Elem e) = Elem $ f e
mapNodeElement _ n = n


element :: Text -> Element
element nm = Element False nm mempty mempty mempty


instance Attributable (Html a) where
  modAttributes f h =
    mapElement (\elm -> elm{attributes = f elm.attributes}) h


tag :: Text -> Html () -> Html ()
tag nm (Html _ content) = do
  Html () [Elem $ (element nm){content}]


text :: Text -> Html ()
text t = Html () [Text t]


none :: Html ()
none = pure ()


raw :: Text -> Html ()
raw t = Html () [Raw t]


instance Styleable (Html a) where
  modCSS f h =
    mapElement (\elm -> elm{css = f elm.css}) h


htmlCSSRules :: Html a -> Map Selector Rule
htmlCSSRules (Html _ ns) = mconcat $ fmap nodeCSSRules ns


nodeCSSRules :: Node -> Map Selector Rule
nodeCSSRules = \case
  Elem elm -> elementCSSRules elm
  _ -> []


elementCSSRules :: Element -> Map Selector Rule
elementCSSRules elm =
  ruleMap elm.css <> (mconcat $ fmap nodeCSSRules elm.content)


elementClasses :: Element -> [ClassName]
elementClasses elm =
  -- fmap (.className) $ elm.css <> M.elems elm.styles
  L.sort $ fmap ruleClassName $ elm.css

-- -- TEST --------------------------
--
-- asdf :: (Attributable h) => Attributes h -> Attributes h
-- asdf = att "asdf" "hello"
--
--
-- asdf2 :: Attributes (Html a -> Html a) -> Attributes (Html a -> Html a)
-- asdf2 = att "asdf" "hello"
--
--
-- test :: Html ()
-- test = tag "div" @ asdf2 $ none
