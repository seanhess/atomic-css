module Web.Atomic.CSS.Text where

import Data.Char (toLower)
import Data.Text (Text)
import Web.Atomic.Types


bold :: (Styleable h) => CSS h -> CSS h
bold = utility @Text "bold" "font-weight" "bold"


fontSize :: (Styleable h) => Length -> CSS h -> CSS h
fontSize n = utility ("fs" -. n) "font-size" n


-- | Set the text color. See 'Web.View.Types.ToColor'
color :: (Styleable h) => (ToColor clr) => clr -> CSS h -> CSS h
color c = utility ("clr" -. colorName c) "color" (colorValue c)


italic :: (Styleable h) => CSS h -> CSS h
italic = utility @Text "italic" "font-style" "italic"


underline :: (Styleable h) => CSS h -> CSS h
underline = utility @Text "underline" "text-decoration" "underline"


data Align
  = AlignCenter
  | AlignLeft
  | AlignRight
  | AlignJustify
  deriving (Show, ToClassName)
instance ToStyleValue Align where
  toStyleValue a = StyleValue . fmap toLower $ drop 5 $ show a


textAlign :: (Styleable h) => Align -> CSS h -> CSS h
textAlign a =
  utility ("ta" -. a) "text-align" a


data TextWrap
instance PropertyStyle TextWrap Wrap


--   = Balance
--   | Pretty
--   | Stable
--   deriving (Show, ToStyleValue, ToClassName)

textWrap :: (PropertyStyle TextWrap w, ToClassName w, ToStyleValue w, Styleable h) => w -> CSS h -> CSS h
textWrap w =
  utility ("twrap" -. w) "text-wrap" (propertyStyle @TextWrap w)
