module Web.Atomic.Types.ClassName where

import Data.String (IsString (..))
import Data.Text (Text, pack)
import Data.Text qualified as T
import Numeric (showFFloat)


-- | A class name
newtype ClassName = ClassName
  { text :: Text
  }
  deriving newtype (Eq, Ord, Show, Monoid, Semigroup)


instance IsString ClassName where
  fromString = className . pack


-- | Create a class name, escaping special characters
className :: Text -> ClassName
className = ClassName . T.toLower . T.map noDot
 where
  noDot '.' = '-'
  noDot c = c


-- | Convert a type into a className segment to generate unique compound style names based on the value
class ToClassName a where
  toClassName :: a -> ClassName
  default toClassName :: (Show a) => a -> ClassName
  toClassName = className . pack . show


instance ToClassName Int
instance ToClassName Text where
  toClassName = className
instance ToClassName Float where
  toClassName f = className $ pack $ showFFloat (Just 3) f ""
instance ToClassName ClassName where
  toClassName = id
instance ToClassName [ClassName] where
  toClassName cs = ClassName $ T.intercalate "-" $ fmap (.text) cs
instance ToClassName () where
  toClassName _ = ""


-- | Hyphenate classnames
(-.) :: (ToClassName a) => ClassName -> a -> ClassName
cn -. a = joinClassSegments "-" cn (toClassName a)


infixl 7 -.


joinClassSegments :: Text -> ClassName -> ClassName -> ClassName
joinClassSegments _ "" cn = cn
joinClassSegments _ cn "" = cn
joinClassSegments sep (ClassName cn1) (ClassName cn2) =
  ClassName $ cn1 <> sep <> cn2


addClassState :: (ToClassName a) => a -> ClassName -> ClassName
addClassState a cn = joinClassSegments ":" (toClassName a) cn


-- appendClassSegments :: (ToClassName a) => [a] -> ClassName -> ClassName
-- appendClassSegments as cn = foldl (flip appendClassSegment) cn as

classesAttValue :: [ClassName] -> Text
classesAttValue clss =
  T.unwords $ fmap (.text) clss
