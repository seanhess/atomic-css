{-# LANGUAGE LambdaCase #-}

module Web.Atomic.Types.Selector where

import Data.String (IsString (..))
import Data.Text (Text, pack)
import Data.Text qualified as T
import GHC.Exts (IsList (..))
import Web.Atomic.Types.ClassName


-- Selector ---------------------------------------------------------------------

newtype Selector = Selector {text :: Text}
  deriving (Eq, Ord, Show)
  deriving newtype (IsString, Semigroup, Monoid)


selector :: ClassName -> Selector
selector (ClassName c) =
  Selector $ "." <> clean c
 where
  clean t = T.replace ":" "\\:" t


-- Pseudo -------------------------------------------------------------------------

{- | Psuedos allow for specifying styles that only apply in certain conditions. See `Web.Atomic.Style.hover` etc

> el (color Primary . hover (color White)) "hello"
-}
data Pseudo = Pseudo {name :: ClassName, suffix :: Selector}
  deriving (Show, Eq, Ord)


instance IsString Pseudo where
  fromString s =
    let c = fromString s
     in Pseudo c (":" <> Selector (pack s))


instance ToClassName Pseudo where
  toClassName p = p.name


-- pseudoText :: Pseudo -> Text
-- pseudoText p = T.toLower $ pack $ show p

-- Media ---------------------------------------------------------------------

newtype MediaQuery = MediaQuery {conditions :: [Text]}
  deriving (Eq, Show)
  deriving newtype (Monoid, Semigroup)
instance IsString MediaQuery where
  fromString s = MediaQuery [pack s]
instance IsList MediaQuery where
  type Item MediaQuery = Text
  fromList = MediaQuery
  toList = (.conditions)


-- | Media allows for responsive designs that change based on characteristics of the window. See [Layout Example](https://github.com/seanhess/atomic-css/blob/master/example/Example/Layout.hs)
data Media
  = MinWidth Int
  | MaxWidth Int
  deriving (Eq, Ord, Show)


instance ToClassName Media where
  toClassName = \case
    MinWidth mn ->
      className $ "mmnw" <> (pack $ show mn)
    MaxWidth mx ->
      className $ "mmxw" <> (pack $ show mx)


mediaCriteria :: Media -> MediaQuery
mediaCriteria (MinWidth n) = MediaQuery ["min-width: " <> (pack $ show n) <> "px"]
mediaCriteria (MaxWidth n) = MediaQuery ["max-width: " <> (pack $ show n) <> "px"]
