{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}

{- |
Module:      Web.Atomic.CSS.Layout
Copyright:   (c) 2023 Sean Hess
License:     BSD3
Maintainer:  Sean Hess <seanhess@gmail.com>
Stability:   experimental
Portability: portable

We can intuitively create layouts by combining of 'flexRow', 'flexCol', 'grow', and 'stack'


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

Also see 'Web.Atomic.Html.Tag.col',  'Web.Atomic.Html.Tag.row', and  'Web.Atomic.Html.Tag.space'
-}
module Web.Atomic.CSS.Layout where

import Web.Atomic.CSS.Box (sides')
import Web.Atomic.Types


{- | Lay out children in a column. See 'Web.Atomic.Html.Tag.col'

> el ~ flexCol $ do
>    el "Top"
>    el " - " ~ grow
>    el "Bottom"
-}


flexCol :: (Styleable h) => CSS h -> CSS h
flexCol =
  utility
    "col"
    [ "display" :. "flex"
    , "flex-direction" :. style Column
    ]

{- | Lay out children in a row. See 'Web.Atomic.Html.Tag.row'

> el ~ flexRow $ do
>    el "Left"
>    el " - " ~ grow
>    el "Right"
-}
flexRow :: (Styleable h) => CSS h -> CSS h
flexRow =
  utility
    "row"
    [ "display" :. "flex"
    , "flex-direction" :. style Row
    ]




-- | Grow to fill the available space in the parent 'flexRow' or 'flexCol'
grow :: (Styleable h) => CSS h -> CSS h
grow = utility "grow" ["flex-grow" :. "1"]


-- space :: (IsHtml h, AppliedParent h ~ h, Styleable h) => h
-- space = el ~ grow $ none

{- | Stack children on top of each other as layers. Each layer has the full width. See 'popup'

> el ~ stack $ do
>   el "Background"
>   el ~ bg Black . opacity 0.5 $ "Overlay"
-}
stack :: (Styleable h) => CSS h -> CSS h
stack =
  container . absChildren
 where
  container =
    utility
      "stack"
      [ "position" :. "relative"
      , "display" :. "grid"
      , "overflow" :. "visible"
      ]

  absChildren =
    css
      "stack-child"
      ".stack-child > *"
      [ "grid-area" :. "1 / 1"
      , "min-height" :. "fit-content"
      ]


{- | Place an element above others, out of the flow of the page

> el ~ stack $ do
>   input @ value "Autocomplete Box"
>   el ~ popup (TL 10 10) $ do
>     el "Item 1"
>     el "Item 2"
>     el "Item 3"
> el "This would be covered by the menu"
-}
popup :: (Styleable h) => Sides Length -> CSS h -> CSS h
popup sides =
  position Absolute . inset sides


-- | Set 'top', 'bottom', 'right', and 'left' all at once
inset :: (Styleable h) => Sides Length -> CSS h -> CSS h
inset = sides' (\n -> top n . right n . bottom n . left n) top right bottom left


top :: (Styleable h) => Length -> CSS h -> CSS h
top l = utility ("top" -. l) ["top" :. style l]


bottom :: (Styleable h) => Length -> CSS h -> CSS h
bottom l = utility ("bottom" -. l) ["bottom" :. style l]


right :: (Styleable h) => Length -> CSS h -> CSS h
right l = utility ("right" -. l) ["right" :. style l]


left :: (Styleable h) => Length -> CSS h -> CSS h
left l = utility ("left" -. l) ["left" :. style l]


data FlexDirection
  = Row
  | Column
  deriving (Show, ToStyle)
instance ToClassName FlexDirection where
  toClassName Row = "row"
  toClassName Column = "col"


flexDirection :: (Styleable h) => FlexDirection -> CSS h -> CSS h
flexDirection dir = utility (toClassName dir) ["flex-direction" :. style dir]


data FlexWrap
  = WrapReverse
  deriving (Show, ToStyle)
instance PropertyStyle FlexWrap FlexWrap
instance PropertyStyle FlexWrap Wrap
instance ToClassName FlexWrap where
  toClassName WrapReverse = "rev"


{- | Set the flex-wrap

@
el ~ flexWrap 'WrapReverse' $ do
  el "one"
  el "two"
  el "three"
el ~ flexWrap 'Wrap' $ do
  el "one"
  el "two"
  el "three"
@
-}
flexWrap :: (PropertyStyle FlexWrap w, ToClassName w, Styleable h) => w -> CSS h -> CSS h
flexWrap w =
  utility ("fwrap" -. w) ["flex-wrap" :. propertyStyle @FlexWrap w]


{- | position:absolute, relative, etc. See 'stack' and 'popup' for a higher-level interface

@
tag "nav" ~ position Fixed . height 100 $ "Navigation bar"
tag "div" ~ flexCol . margin (T 100) $ "Main Content"
@
-}
position :: (Styleable h) => Position -> CSS h -> CSS h
position p = utility ("pos" -. p) ["position" :. style p]


data Position
  = Absolute
  | Fixed
  | Sticky
  | Relative
  deriving (Show, ToClassName, ToStyle)


zIndex :: (Styleable h) => Int -> CSS h -> CSS h
zIndex n = utility ("z" -. n) ["z-index" :. style n]


{- | Set container display

@
el ~ (display 'None') $ "none"
el ~ (display 'Block') $ "block"
@
-}
display :: (PropertyStyle Display d, ToClassName d, Styleable h) => d -> CSS h -> CSS h
display disp =
  utility ("disp" -. disp) ["display" :. propertyStyle @Display disp]


data Display
  = Block
  | Flex
  deriving (Show, ToClassName, ToStyle)
instance PropertyStyle Display Display
instance PropertyStyle Display None


data Visibility
  = Visible
  | Hidden
  deriving (Show, ToClassName, ToStyle)


visibility :: Styleable h => Visibility -> CSS h -> CSS h
visibility v = utility ("vis" -. v) ["visibility" :. style v]

{- | Set to specific width

> el ~ width 100 $ "100px"
> el ~ width (PxRem 100) $ "100px"
> el ~ width (Pct 50) $ "50pct"
-}
width :: (Styleable h) => Length -> CSS h -> CSS h
width n = utility ("w" -. n) [ "width" :. style n ]


height :: (Styleable h) => Length -> CSS h -> CSS h
height n = utility ("h" -. n) [ "height" :. style n ]


-- | Allow width to grow to contents but not shrink any smaller than value
minWidth :: (Styleable h) => Length -> CSS h -> CSS h
minWidth n =
  utility ("mw" -. n) ["min-width" :. style n]


-- | Allow height to grow to contents but not shrink any smaller than value
minHeight :: (Styleable h) => Length -> CSS h -> CSS h
minHeight n =
  utility ("mh" -. n) ["min-height" :. style n]


data Overflow
  = Scroll
  | Clip
  deriving (Show, ToStyle, ToClassName)
instance PropertyStyle Overflow Overflow
instance PropertyStyle Overflow Auto
instance PropertyStyle Overflow Visibility


-- | Control how an element clips content that exceeds its bounds 
overflow :: (PropertyStyle Overflow o, ToClassName o, Styleable h) => o -> CSS h -> CSS h
overflow o = utility ("over" -. o) ["overflow" :. propertyStyle @Overflow o]
