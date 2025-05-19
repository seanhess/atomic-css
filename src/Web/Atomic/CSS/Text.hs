module Web.Atomic.CSS.Text where

import Data.Char (toLower)
import Web.Atomic.Types


bold :: (Styleable h) => CSS h -> CSS h
bold = utility "bold" ["font-weight" :. "bold"]


fontSize :: (Styleable h) => Length -> CSS h -> CSS h
fontSize n = utility ("fs" -. n) ["font-size" :. style n]


color :: (Styleable h) => (ToColor clr) => clr -> CSS h -> CSS h
color c = utility ("clr" -. colorName c) ["color" :. style (colorValue c)]


italic :: (Styleable h) => CSS h -> CSS h
italic = utility "italic" ["font-style" :. "italic"]


underline :: (Styleable h) => CSS h -> CSS h
underline = utility "underline" ["text-decoration" :. "underline"]


data Align
  = AlignCenter
  | AlignLeft
  | AlignRight
  | AlignJustify
  deriving (Show, ToClassName)
instance ToStyle Align where
  style a = Style . fmap toLower $ drop 5 $ show a


textAlign :: (Styleable h) => Align -> CSS h -> CSS h
textAlign a =
  utility ("ta" -. a) ["text-align" :. style a]


data TextWrap
instance PropertyStyle TextWrap Wrap


--   = Balance
--   | Pretty
--   | Stable
--   deriving (Show, ToStyleValue, ToClassName)

textWrap :: (PropertyStyle TextWrap w, ToClassName w, Styleable h) => w -> CSS h -> CSS h
textWrap w =
  utility ("twrap" -. w) ["text-wrap" :. propertyStyle @TextWrap w]
