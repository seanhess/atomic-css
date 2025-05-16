{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Atomic.Types.Style where

import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Numeric (showFFloat)
import Text.Casing (kebab)
import Web.Atomic.Types.ClassName (ToClassName (..), className)


newtype Property = Property Text
  deriving newtype (Show, Eq, Ord, IsString)


data Declaration = Property :. Style
  deriving (Show, Ord, Eq)


newtype Style = Style String
  deriving newtype (IsString, Show, Eq, Monoid, Semigroup, Ord)


-- | Convert a type to a css style property value
class ToStyle a where
  style :: a -> Style
  default style :: (Show a) => a -> Style
  style = Style . kebab . show


instance ToStyle String where
  style = Style
instance ToStyle Text where
  style = Style . unpack
instance ToStyle Int
instance ToStyle Float where
  -- this does not convert to a percent, just a ratio
  style n = Style $ showFFloat (Just 2) n ""
instance ToStyle Style where
  style = id


-- uniquely set the style value based on the property in question
class PropertyStyle property value where
  propertyStyle :: value -> Style
  default propertyStyle :: (ToStyle value) => value -> Style
  propertyStyle = style


data None = None
  deriving (Show, ToClassName, ToStyle)


-- -- | Convert a type to a prop name
-- class ToProp a where
--   toProp :: a -> Text
--   default toProp :: (Show a) => a -> Text
--   toProp = pack . kebab . show

data Length
  = PxRem PxRem
  | Pct Float
  deriving (Show)


instance ToClassName Length where
  toClassName (PxRem p) = toClassName p
  toClassName (Pct p) = toClassName p


-- | Px, converted to Rem. Allows for the user to change the document font size and have the app scale accordingly. But allows the programmer to code in pixels to match a design
newtype PxRem = PxRem' Int
  deriving newtype (Show, ToClassName, Num, Eq, Integral, Real, Ord, Enum)


instance Num Length where
  PxRem p1 + PxRem p2 = PxRem $ p1 + p2
  -- 10 + 10% = 10 + 10% of 10 = 11
  PxRem p1 + Pct pct = PxRem $ round $ (fromIntegral p1) * (1 + pct)
  Pct pct + PxRem p1 = PxRem p1 + Pct pct
  Pct p1 + Pct p2 = Pct $ p1 + p2


  PxRem p1 * PxRem p2 = PxRem $ p1 + p2
  PxRem p1 * Pct pct = PxRem $ round $ (fromIntegral p1) * pct
  Pct pct * PxRem p1 = PxRem p1 * Pct pct
  Pct p1 * Pct p2 = Pct $ p1 * p2


  abs (PxRem a) = PxRem (abs a)
  abs (Pct a) = Pct (abs a)
  signum (PxRem a) = PxRem (signum a)
  signum (Pct a) = Pct (signum a)
  negate (PxRem a) = PxRem (negate a)
  negate (Pct a) = Pct (negate a)
  fromInteger n = PxRem (fromInteger n)


instance ToStyle PxRem where
  style (PxRem' 0) = "0px"
  style (PxRem' 1) = "1px"
  style (PxRem' n) = Style $ showFFloat (Just 3) ((fromIntegral n :: Float) / 16.0) "" <> "rem"


instance ToStyle Length where
  style (PxRem p) = style p
  style (Pct n) = Style $ showFFloat (Just 1) (n * 100) "" <> "%"


-- | Milliseconds, used for transitions
newtype Ms = Ms Int
  deriving (Show)
  deriving newtype (Num, ToClassName)


instance ToStyle Ms where
  style (Ms n) = Style $ show n <> "ms"


data Wrap
  = Wrap
  | NoWrap
  deriving (Show, ToClassName)
instance ToStyle Wrap where
  style Wrap = "wrap"
  style NoWrap = "nowrap"


{- | Options for styles that support specifying various sides. This has a "fake" Num instance to support literals

> border 5
> border (X 2)
> border (TRBL 0 5 0 0)
-}
data Sides a
  = All a
  | TRBL a a a a
  | X a
  | Y a
  | XY a a
  | T a
  | R a
  | B a
  | L a
  | TR a a
  | TL a a
  | BR a a
  | BL a a


-- Num instance is just to support literals
instance (Num a) => Num (Sides a) where
  a + _ = a
  a * _ = a
  abs a = a
  negate a = a
  signum a = a
  fromInteger n = All (fromInteger n)


-- ** Colors


{- | ToColor allows you to create a type containing your application's colors:

> data AppColor
>   = White
>   | Primary
>   | Dark
>
> instance ToColor AppColor where
>   colorValue White = "#FFF"
>   colorValue Dark = "#333"
>   colorValue Primary = "#00F"
>
> hello :: View c ()
> hello = el (bg Primary . color White) "Hello"
-}
class ToColor a where
  colorValue :: a -> HexColor
  colorName :: a -> Text
  default colorName :: (Show a) => a -> Text
  colorName = T.toLower . pack . show


-- | Hexidecimal Color. Can be specified with or without the leading '#'. Recommended to use an AppColor type instead of manually using hex colors. See 'Web.Atomic.Types.ToColor'
newtype HexColor = HexColor Text
  deriving (Show)


instance ToColor HexColor where
  colorValue c = c
  colorName (HexColor a) = T.dropWhile (== '#') a


instance ToStyle HexColor where
  style (HexColor s) = Style $ "#" <> unpack (T.dropWhile (== '#') s)


instance IsString HexColor where
  fromString = HexColor . T.dropWhile (== '#') . T.pack


instance ToClassName HexColor where
  toClassName = className . colorName

-- (.:) :: (ToStyle a) => Property -> Style -> Declaration
-- cn .: v =
--   Declaration cn (toStyleValue v)
