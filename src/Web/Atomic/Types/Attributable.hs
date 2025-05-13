module Web.Atomic.Types.Attributable where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)


type Name = Text
type AttValue = Text


newtype Attributes h = Attributes (Map Name AttValue)
  deriving newtype (Monoid, Semigroup)


-- | Add Atts
class Attributable h where
  (@) :: h -> (Attributes h -> Attributes h) -> h
  h @ f =
    flip modAttributes h $ \m ->
      let Attributes atts = f $ Attributes m
      in atts


  modAttributes :: (Map Name AttValue -> Map Name AttValue) -> h -> h


infixl 5 @


instance {-# OVERLAPPABLE #-} (Attributable a, Attributable b) => Attributable (a -> b) where
  (@) :: (a -> b) -> (Attributes (a -> b) -> Attributes (a -> b)) -> (a -> b)
  hh @ f = \content ->
    hh content @ \(Attributes m) ->
      let Attributes m2 = f $ Attributes m
       in Attributes m2


  modAttributes f hh = \content ->
    modAttributes f $ hh content


instance Attributable (Map Name AttValue) where
  modAttributes f m = f m


instance Attributable (Attributes h) where
  modAttributes f (Attributes m) = Attributes $ f m


att :: (Attributable h) => Name -> AttValue -> Attributes h -> Attributes h
att n av (Attributes m) =
  Attributes $ M.insert n av m

-- propKey :: (Styleable a) => PropKey -> Rule -> Styles a -> Styles a
-- propKey pk r (Styles h) = Styles $ addStyle pk r h

{-
newtype Fake c a = Fake [a]
  deriving newtype (Functor, Applicative, Monad)

-- type Styles a = Styles' a a
-- type Attributes a = Attributes a a

onClick :: (Attributable a) => Attributes a -> Attributes a
onClick = att "one" "two"

bold :: Styles a -> Styles a
bold = undefined

instance Attributable (Fake c ()) where
  -- type AttsFor (Fake c ()) = Ats c
  f @ a = undefined

instance Styleable (Fake c ()) where
  -- type A c ()) = Atts c
  -- type StylesFor (Fake c ()) = Stl c
  f ~ a = undefined

tag :: Text -> Fake c () -> Fake c ()
tag = undefined

el :: Fake c () -> Fake c ()
el = tag "div"

none :: Fake c ()
none = undefined

img :: Fake c ()
img = tag "img" none

text :: Text -> Fake c ()
text = undefined

-- ultimately it doesn't know how to resolve it because it can't look up what atts is?
test :: Fake c ()
test = do
  el @ att "one" "two" ~ bold . bold $ do
    el $ do
      text "hello"
      img @ att "src" "woot"
-}

-- -- for a given attributes, how do we convert them?
-- class (Attributable h) => ToAttributes atts h where
--   toAttributes :: h -> atts -> AttsFor h
--
--
-- (!) :: (ToAttributes atts h, Attributable h) => h -> (atts -> atts) -> h
-- a ! b = _
-- infixl 5 !
--
--
-- instance (ToAttributes (Ats c) (Fake c () -> Fake c ())) where
--   toAttributes = _
--
--
-- --
-- --
-- -- instance Styleable (Styles c) (Fake c () -> Fake c ()) where
-- --   -- type Attributes (Fake c () -> Fake c ()) = Atts c
-- --   f ~ a = undefined
-- --
-- --
-- -- instance Attributable (Atts c) (Fake c () -> Fake c ()) where
-- --   -- type Attributes (Fake c () -> Fake c ()) = Atts c
-- --   f @ a = undefined
-- --
-- --
-- --

--
--
-- -- What if attributes, styles, etc were the same for any type?
-- -- mapAttributes :: (SetAttributes h -> SetAttributes h) -> h -> h
--
-- -- default mapAttributes :: (SetAttributes h ~ Attributes h) => (SetAttributes h -> SetAttributes h) -> h -> h
-- -- mapAttributes fas html = (fas (Attributes html)).html
--
-- -- mapStyles :: (SetStyles h -> SetStyles h) -> h -> h
--
-- -- default mapStyles :: (SetStyles h ~ Styles h) => (SetStyles h -> SetStyles h) -> h -> h
-- -- mapStyles fas html = (fas (Styles html)).html
--
-- -- instance (Attributable h) => Attributable (h -> h) where
-- --   type SetAttributes (h -> h) = SetAttributes h
-- --   type SetStyles (h -> h) = SetStyles h
--
-- -- mapAttributes f parent = \content ->
-- --   mapAttributes f (parent content)
-- -- mapStyles f parent = \content ->
-- --   mapStyles f (parent content)
--
-- -- instance (Monad m) => Attributable (m ()) where
-- --   type SetAttributes (m ()) = Attributes (m ())
-- --   type SetStyles (m ()) = Styles (m ())
--
-- --
-- --

-- -- instance HasAttributes (Fake c ()) where
-- --   addAttribute = undefined
-- --   addStyle = undefined
-- --   addCSSRule = undefined
--
-- -- newtype Styl c = Styl (Fake c ())
-- -- newtype Atts c = Atts (Fake c ())
--
-- -- instance Attributable (Fake c ()) where
-- --   type SetStyles (Fake c ()) = Styl c
-- --   type SetAttributes (Fake c ()) = Atts c
-- --   addAttribute = undefined
-- --   addStyle = undefined
-- --   addCSSRule = undefined
--
-- -- mapStyles = undefined
-- -- mapAttributes = undefined
-- --
-- --
-- --
-- --
-- -- TRADEOFFS
-- --
-- --
-- -- 1. if everything is (h -> h), you can't make `hover` throw a type error. It's most like the current version
-- -- 2. make different versions for Html () -> Html (). Redefine everything. (More work, but most convenient? Better type errors)
