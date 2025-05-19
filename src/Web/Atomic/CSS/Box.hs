module Web.Atomic.CSS.Box where

import Web.Atomic.Types


-- | Cut off content that goes beyond the element size
clip :: (Styleable h) => CSS h -> CSS h
clip =
  utility
    "clip"
    [ "white-space" :. "nowrap"
    , "overflow" :. "hidden"
    , "text-overflow" :. "ellipsis"
    ]


{- | Make a fixed 'layout' by putting 'scroll' on a child-element

> document = el ~ flexRow . fillViewport $ do
>   tag "nav" ~ width 300 $ "Sidebar"
>   tag "div" ~ scroll . grow $ "Main Content"
-}
scroll :: (Styleable h) => CSS h -> CSS h
scroll = utility "scroll" ["overflow" :. "auto"]


{- | Space surrounding the children of the element

To create even spacing around and between all elements combine with 'gap'

> el ~ flexCol . pad 10 . gap 10 $ do
>   el "one"
>   el "two"
>   el "three"
-}
pad :: (Styleable h) => Sides Length -> CSS h -> CSS h
pad =
  sides "p" ("padding" <>)


-- | The space between child elements. See 'pad'
gap :: (Styleable h) => Length -> CSS h -> CSS h
gap n = utility ("gap" -. n) ["gap" :. style n]


-- | Element margin. Using 'gap' and 'pad' on parents is more intuitive and usually makes margin redundant
margin :: (Styleable h) => Sides Length -> CSS h -> CSS h
margin =
  sides "m" ("margin" <>)


{- | Add a drop shadow to an element

> input ~ shadow Inner $ "Inset Shadow"
> button ~ shadow () $ "Click Me"
-}
shadow :: (Styleable h, PropertyStyle Shadow a, ToClassName a) => a -> CSS h -> CSS h
shadow a =
  utility ("shadow" -. a) ["box-shadow" :. propertyStyle @Shadow a]


data Shadow
data Inner = Inner
  deriving (Show, ToClassName)


instance PropertyStyle Shadow () where
  propertyStyle _ = "0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1);"
instance PropertyStyle Shadow None where
  propertyStyle _ = "0 0 #0000;"
instance PropertyStyle Shadow Inner where
  propertyStyle _ = "inset 0 2px 4px 0 rgb(0 0 0 / 0.05);"


-- | Set the background color. See 'ToColor'
bg :: (ToColor clr, Styleable h) => clr -> CSS h -> CSS h
bg c = utility ("bg" -. colorName c) ["background-color" :. style (colorValue c)]


-- | Round the corners of the element
rounded :: (Styleable h) => Length -> CSS h -> CSS h
rounded n = utility ("rnd" -. n) ["border-radius" :. style n]


{- | Set a border around the element

> el ~ border 1 $ "all sides"
> el ~ border (X 1) $ "only left and right"
-}
border :: (Styleable h) => Sides PxRem -> CSS h -> CSS h
border s = borderWidth s . borderStyle Solid


borderStyle :: (Styleable h) => BorderStyle -> CSS h -> CSS h
borderStyle s = utility ("brds" -. s) ["border-style" :. style s]


data BorderStyle
  = Solid
  | Dashed
  deriving (Show, ToStyle, ToClassName)


borderWidth :: (Styleable h) => Sides PxRem -> CSS h -> CSS h
borderWidth =
  sides "brd" prop
 where
  prop "" = "border-width"
  prop p = "border" <> p <> "-width"


-- | Set a border color. See 'ToColor'
borderColor :: (ToColor clr, Styleable h) => clr -> CSS h -> CSS h
borderColor c =
  utility ("brdc" -. colorName c) ["border-color" :. style (colorValue c)]


opacity :: (Styleable h) => Float -> CSS h -> CSS h
opacity n =
  utility ("opacity" -. n) ["opacity" :. style n]


-- | utilities for every side with (Sides a)
sides :: (Styleable h, ToStyle a, ToClassName a, Num a) => ClassName -> (Property -> Property) -> Sides a -> CSS h -> CSS h
sides c toProp =
  sides'
    (\a -> utility (c -. a) [toProp "" :. style a])
    (\a -> utility (c <> "t" -. a) [toProp "-top" :. style a])
    (\a -> utility (c <> "r" -. a) [toProp "-right" :. style a])
    (\a -> utility (c <> "b" -. a) [toProp "-bottom" :. style a])
    (\a -> utility (c <> "l" -. a) [toProp "-left" :. style a])


-- | case analysis for (Sides a)
sides' :: (Styleable h, ToStyle a, ToClassName a, Num a) => (a -> CSS h -> CSS h) -> (a -> CSS h -> CSS h) -> (a -> CSS h -> CSS h) -> (a -> CSS h -> CSS h) -> (a -> CSS h -> CSS h) -> Sides a -> CSS h -> CSS h
sides' all_ top right bottom left s =
  case s of
    (All n) -> all_ n
    (Y n) -> top n . bottom n
    (X n) -> left n . right n
    (XY x y) -> top y . bottom y . left x . right x
    (TRBL t r b l) ->
      top t . right r . bottom b . left l
    (T x) -> top x
    (R x) -> right x
    (B x) -> bottom x
    (L x) -> left x
    (TR t r) -> top t . right r
    (TL t l) -> top t . left l
    (BR b r) -> bottom b . right r
    (BL b l) -> bottom b . left l
