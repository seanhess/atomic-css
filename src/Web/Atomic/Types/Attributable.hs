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
  -- | Apply an attribute to some html
  --
  -- > el @ att "id" "main-content" $ do
  -- >   tag "img" @ att "src" "logo.png"
  -- >   tag "input" @ placeholder "message" ~ border 1
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


  modAttributes f hh content =
    modAttributes f $ hh content


instance Attributable (Map Name AttValue) where
  modAttributes f = f


instance Attributable (Attributes h) where
  modAttributes f (Attributes m) = Attributes $ f m


att :: (Attributable h) => Name -> AttValue -> Attributes h -> Attributes h
att n av (Attributes m) =
  Attributes $ M.insert n av m
