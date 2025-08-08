{- |
Module:      Web.Atomic.CSS
Copyright:   (c) 2023 Sean Hess
License:     BSD3
Maintainer:  Sean Hess <seanhess@gmail.com>
Stability:   experimental
Portability: portable

Type-safe Atomic CSS with composable css utility classes and intuitive layouts. Inspired by Tailwindcss and Elm-UI

@
import Web.Atomic

example = do
  'el' ~ 'flexCol' . 'gap' 10 $ do
    'el' ~ 'bold' . 'fontSize' 32 $ "My page"
    'el' "Hello!"
@

See [Web.Atomic](Web-Atomic.html) for a complete introduction
-}
module Web.Atomic.CSS
  ( -- * Atomic CSS
    Styleable ((~))
  , utility
  , css
  , cls

    -- * CSS Utilities

    -- ** Layout
  , display
  , Display (..)
  , visibility
  , Visibility (..)
  , width
  , height
  , minWidth
  , minHeight
  , position
  , Position (..)
  , inset
  , top
  , bottom
  , right
  , left
  , overflow
  , Overflow (..)

    -- ** Flexbox
    -- $flexbox
  , flexRow
  , flexCol
  , grow
  , flexDirection
  , FlexDirection (..)
  , flexWrap
  , FlexWrap (..)

    -- ** Window
  , zIndex

    -- ** Stack
  , stack
  , popup

    -- ** Box Model
  , pad
  , gap
  , margin
  , bg
  , border
  , borderWidth
  , borderColor
  , borderStyle
  , BorderStyle (..)
  , rounded
  , opacity
  , shadow
  , Shadow
  , Inner (..)

    -- ** Text
  , bold
  , fontSize
  , color
  , italic
  , underline
  , textAlign
  , Align (..)
  , whiteSpace
  , WhiteSpace (..)

    -- ** CSS Transitions
  , transition
  , TransitionProperty (..)

    -- ** Elements
  , list
  , ListType (..)
  , pointer

    -- ** Selector Modifiers
  , hover
  , active
  , even
  , odd
  , descendentOf
  , media
  , Media (..)

    -- ** Colors
  , ToColor (..)
  , HexColor (..)

    -- * CSS Reset
  , cssResetEmbed

    -- * Types
  , Property
  , Declaration (..)
  , Style
  , ToStyle (..)
  , PropertyStyle (..)
  , None (..)
  , Auto (..)
  , Normal (..)
  , Length (..)
  , PxRem (..)
  , Ms (..)
  , Wrap (..)
  , Sides (..)
  , CSS

    -- * Other
  , declarations
  , rules
  ) where

import Web.Atomic.CSS.Box hiding (sides, sides')
import Web.Atomic.CSS.Layout
import Web.Atomic.CSS.Reset
import Web.Atomic.CSS.Select hiding (addAncestor, addMedia, addPseudo)
import Web.Atomic.CSS.Text
import Web.Atomic.CSS.Transition
import Web.Atomic.Types
import Prelude hiding (even, odd, truncate)


{- | Set the list style of an item

> tag "ol" $ do
>   tag "li" ~ list Decimal $ "one"
>   tag "li" ~ list Decimal $ "two"
>   tag "li" ~ list Decimal $ "three"
-}
list :: (ToClassName l, PropertyStyle ListType l, Styleable h) => l -> CSS h -> CSS h
list a =
  utility ("list" -. a) ["list-style-type" :. propertyStyle @ListType a]


data ListType
  = Decimal
  | Disc
  deriving (Show, ToClassName, ToStyle)
instance PropertyStyle ListType ListType
instance PropertyStyle ListType None


{- | Use a button-like cursor when hovering over the element

Button-like elements:

> btn = pointer . bg Primary . hover (bg PrimaryLight)
>
> options = do
>   el ~ btn $ "Login"
>   el ~ btn $ "Sign Up"
-}
pointer :: (Styleable h) => CSS h -> CSS h
pointer = utility "pointer" ["cursor" :. "pointer"]


{- $use

See
-}


{- $flexbox

We can intuitively create layouts by combining 'flexRow', 'flexCol', 'grow', and 'stack'

@
holygrail = do
  el ~ flexCol . grow $ do
    el ~ flexRow $ "Top Bar"
    el ~ flexRow . grow $ do
      el ~ flexCol $ "Left Sidebar"
      el ~ flexCol . grow $ "Main Content"
      el ~ flexCol $ "Right Sidebar"
    el ~ flexRow $ "Bottom Bar"
@
-}
