{-# LANGUAGE LambdaCase #-}

module Web.Atomic.CSS.Transition where

import Data.Text (Text)
import Web.Atomic.Types


{- | Animate changes to the given property

> el ~ transition 100 (Height 400) $ "Tall"
> el ~ transition 100 (Height 100) $ "Small"
-}
transition :: (Styleable h) => Ms -> TransitionProperty -> CSS h -> CSS h
transition ms = \case
  (Height n) -> trans "height" n
  (Width n) -> trans "width" n
  (BgColor c) -> trans "background-color" c
  (Color c) -> trans "color" c
 where
  trans :: (ToClassName val, ToStyle val, Styleable h) => Text -> val -> CSS h -> CSS h
  trans p val =
    utility
      ("t" -. val -. p -. ms)
      [ "transition-duration" :. style ms
      , "transition-property" :. style p
      , Property p :. style val
      ]


-- You MUST set the height/width manually when you attempt to transition it
data TransitionProperty
  = Width PxRem
  | Height PxRem
  | BgColor HexColor
  | Color HexColor
  deriving (Show)
