module Web.Atomic.CSS
  ( module Web.Atomic.CSS.Select
  , module Web.Atomic.CSS.Box
  , module Web.Atomic.CSS.Text
  , module Web.Atomic.CSS.Transition
  , module Web.Atomic.CSS.Layout
  , module Web.Atomic.Types.Styleable
  , module Web.Atomic.Types.Style
  , Media (..)
  , module Web.Atomic.CSS.Reset
  -- not sure where to put these
  , list
  , ListType (..)
  , pointer
  ) where

import Web.Atomic.CSS.Box
import Web.Atomic.CSS.Layout
import Web.Atomic.CSS.Reset
import Web.Atomic.CSS.Select (active, descendentOf, even, hover, media, odd)
import Web.Atomic.CSS.Text
import Web.Atomic.CSS.Transition
import Web.Atomic.Types
import Web.Atomic.Types.Style
import Web.Atomic.Types.Styleable (CSS, Styleable, cls, css, utility, (~))


{- | Set the list style of an item

> ol id $ do
>   li (list Decimal) "First"
>   li (list Decimal) "Second"
>   li (list Decimal) "Third"
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
> options = row id $ do
>   el btn "Login"
>   el btn "Sign Up"
-}
pointer :: (Styleable h) => CSS h -> CSS h
pointer = utility "pointer" ["cursor" :. "pointer"]
