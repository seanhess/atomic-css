{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Web.Atomic.CSS.Layout where

import Data.Text
import Web.Atomic.Types


{- | We can intuitively create layouts with combinations of 'row', 'col', 'stack', 'grow', and 'space'

Wrap main content in 'layout' to allow the view to consume vertical screen space

@
holygrail :: 'View' c ()
holygrail = 'layout' id $ do
  'row' section "Top Bar"
  'row' 'grow' $ do
    'col' section "Left Sidebar"
    'col' (section . 'grow') "Main Content"
    'col' section "Right Sidebar"
  'row' section "Bottom Bar"
  where section = 'border' 1
@
-}

-- layout :: Html () -> Html ()
-- layout = col @ fillViewport

{- | As `layout` but as a 'Attributes

> holygrail = col root $ do
>   ...
-}
fillViewport :: (Styleable h) => CSS h -> CSS h
fillViewport =
  utility'
    "fill-viewport"
    -- [ ("white-space", "pre")
    [ prop @Text "width" "100vw"
    , prop @Text "height" "100vh"
    , -- not sure if this property is necessary, copied from older code
      prop @Text "min-height" "100vh"
    , prop @Text "z-index" "0"
    ]


{- | Lay out children in a row

> row id $ do
>    el_ "Left"
>    space
>    el_ "Right"
-}
flexRow :: (Styleable h) => CSS h -> CSS h
flexRow =
  utility'
    "row"
    [ Declaration "display" "flex"
    , Declaration "flex-direction" (toStyleValue Row)
    ]


{- | Lay out children in a column.

> col grow $ do
>    el_ "Top"
>    space
>    el_ "Bottom"
-}
flexCol :: (Styleable h) => CSS h -> CSS h
flexCol =
  utility'
    "col"
    [ Declaration "display" "flex"
    , Declaration "flex-direction" (toStyleValue Column)
    ]


{- | Grow to fill the available space in the parent 'Web.View.Layout.row' or 'Web.View.Layout.col'

> row id $ do
>  el grow none
>  el_ "Right"
-}
grow :: (Styleable h) => CSS h -> CSS h
grow = utility @Int "grow" "flex-grow" 1


{- | Space that fills the available space in the parent 'Web.View.Layout.row' or 'Web.View.Layout.col'.


> row id $ do
>  space
>  el_ "Right"

This is equivalent to an empty element with 'grow'

> space = el grow none
-}

-- space :: (IsHtml h, AppliedParent h ~ h, Styleable h) => h
-- space = el ~ grow $ none

{- | Make a fixed 'layout' by putting 'scroll' on a child-element

> document = row root $ do
>   nav (width 300) "Sidebar"
>   col (grow . scroll) "Main Content"
-}
scroll :: (Styleable h) => CSS h -> CSS h
scroll = utility @Text "scroll" "overflow" "auto"


{- | A Nav element
nav :: (IsHtml h) => h -> h
nav = tag "nav"
-}

{- | Stack children on top of each other. Each child has the full width. See 'popup'

> stack id $ do
>   layer id "Background"
>   layer (bg Black . opacity 0.5) "Overlay"
-}
stack :: (Styleable h) => CSS h -> CSS h
stack =
  container . absChildren
 where
  container =
    utility'
      "stack"
      [ prop @Text "position" "relative"
      , prop @Text "display" "grid"
      , prop @Text "overflow" "visible"
      ]

  absChildren =
    css
      "stack-child"
      ".stack-child > *"
      [ prop @Text "grid-area" "1 / 1"
      , prop @Text "min-height" "fit-content"
      ]


{- | This 'layer' is not included in the 'stack' size, and covers content outside of it. If used outside of stack, the popup is offset from the entire page.

> stack id $ do
>   layer id $ input (value "Autocomplete Box")
>   layer (popup (TRBL 50 0 0 0)) $ do
>     el_ "Item 1"
>     el_ "Item 2"
>     el_ "Item 3"
> el_ "This is covered by the menu"
-}
popup :: (Styleable h) => Sides Length -> CSS h -> CSS h
popup sides =
  position Absolute . inset sides


-- | Set top, bottom, right, and left. See 'Web.View.Layout.stack' and 'Web.View.Layout.popup'
inset :: (Styleable h) => Sides Length -> CSS h -> CSS h
inset sides = off sides
 where
  off = \case
    All n -> off (TRBL n n n n)
    Y n -> off (XY 0 n)
    X n -> off (XY n 0)
    XY x y -> off (TRBL y x y x)
    TRBL t r b l -> top t . right r . bottom b . left l
    T x -> top x
    R x -> right x
    B x -> bottom x
    L x -> left x
    TR t r -> top t . right r
    TL t l -> top t . left l
    BR b r -> bottom b . right r
    BL b l -> bottom b . left l


top :: (Styleable h) => Length -> CSS h -> CSS h
top l = utility ("top" -. l) "top" l


bottom :: (Styleable h) => Length -> CSS h -> CSS h
bottom l = utility ("bottom" -. l) "bottom" l


right :: (Styleable h) => Length -> CSS h -> CSS h
right l = utility ("right" -. l) "right" l


left :: (Styleable h) => Length -> CSS h -> CSS h
left l = utility ("left" -. l) "left" l


-- | Hide an element. See 'display'
hide :: (Styleable h) => CSS h -> CSS h
hide = display None


data FlexDirection
  = Row
  | Column
  deriving (Show, ToStyleValue)
instance ToClassName FlexDirection where
  toClassName Row = "row"
  toClassName Column = "col"


flexDirection :: (Styleable h) => FlexDirection -> CSS h -> CSS h
flexDirection dir = utility (toClassName dir) "flex-direction" dir


data FlexWrap
  = WrapReverse
  deriving (Show, ToStyleValue)
instance PropertyStyle FlexWrap FlexWrap
instance PropertyStyle FlexWrap Wrap
instance ToClassName FlexWrap where
  toClassName WrapReverse = "rev"


flexWrap :: (PropertyStyle FlexWrap w, ToClassName w, Styleable h) => w -> CSS h -> CSS h
flexWrap w =
  utility ("fwrap" -. w) "flex-wrap" (propertyStyle @FlexWrap w)


-- | position:absolute, relative, etc. See 'Web.View.Layout.stack' and 'Web.View.Layout.popup'
position :: (Styleable h) => Position -> CSS h -> CSS h
position p = utility ("pos" -. p) "position" p


data Position
  = Absolute
  | Fixed
  | Sticky
  | Relative
  deriving (Show, ToClassName, ToStyleValue)


zIndex :: (Styleable h) => Int -> CSS h -> CSS h
zIndex n = utility ("z" -. n) "z-index" n


{- | Set container display

el (display None) "HIDDEN"
-}
display :: (PropertyStyle Display d, ToClassName d, Styleable h) => d -> CSS h -> CSS h
display disp =
  utility ("disp" -. disp) "display" (propertyStyle @Display disp)


data Display
  = Block
  | Flex
  deriving (Show, ToClassName, ToStyleValue)
instance PropertyStyle Display Display
instance PropertyStyle Display None


hidden :: (Styleable h) => CSS h -> CSS h
hidden = utility' "hidden" [Declaration "visibility" "hidden"]


visible :: (Styleable h) => CSS h -> CSS h
visible = utility' "hidden" [Declaration "visibility" "visible"]


-- what if you set flex-shrink later?
-- it has undefined behavior
--

-- | Set to a specific width
width :: (Styleable h) => Length -> CSS h -> CSS h
width n =
  utility'
    ("w" -. n)
    [ prop "width" n
    , prop @Int "flex-shrink" 0
    ]


-- | Set to a specific height
height :: (Styleable h) => Length -> CSS h -> CSS h
height n =
  utility'
    ("h" -. n)
    [ prop "height" n
    , prop @Int "flex-shrink" 0
    ]


-- | Allow width to grow to contents but not shrink any smaller than value
minWidth :: (Styleable h) => Length -> CSS h -> CSS h
minWidth n =
  utility ("mw" -. n) "min-width" n


-- | Allow height to grow to contents but not shrink any smaller than value
minHeight :: (Styleable h) => Length -> CSS h -> CSS h
minHeight n =
  utility ("mh" -. n) "min-height" n
