module Web.Atomic.CSS.Box where

import Web.Atomic.Types


-- | Cut off the contents of the element
truncate :: (Styleable h) => CSS h -> CSS h
truncate =
  utility
    "truncate"
    [ "white-space" :. "nowrap"
    , "overflow" :. "hidden"
    , "text-overflow" :. "ellipsis"
    ]


{- | Space surrounding the children of the element

To create even spacing around and between all elements:

> col (pad 10 . gap 10) $ do
>   el_ "one"
>   el_ "two"
>   el_ "three"
-}
pad :: (Styleable h) => Sides Length -> CSS h -> CSS h
pad (All n) =
  utility ("p" -. n) ["padding" :. style n]
pad (Y n) = pad (T n) . pad (B n)
pad (X n) = pad (L n) . pad (R n)
pad (XY x y) = pad (X x) . pad (Y y)
pad (TRBL t r b l) =
  pad (T t) . pad (R r) . pad (B b) . pad (L l)
pad (T x) = utility ("pt" -. x) ["padding-top" :. style x]
pad (R x) = utility ("pr" -. x) ["padding-right" :. style x]
pad (B x) = utility ("pb" -. x) ["padding-bottom" :. style x]
pad (L x) = utility ("pl" -. x) ["padding-left" :. style x]
pad (TR t r) = pad (TRBL t r 0 0)
pad (TL t l) = pad (TRBL t 0 0 l)
pad (BR b r) = pad (TRBL 0 r b 0)
pad (BL b l) = pad (TRBL 0 0 b l)


-- | The space between child elements. See 'pad'
gap :: (Styleable h) => Length -> CSS h -> CSS h
gap n = utility ("gap" -. n) ["gap" :. style n]


margin :: (Styleable h) => Sides Length -> CSS h -> CSS h
margin (All n) =
  utility ("m" -. n) ["margin" :. style n]
margin (Y n) = margin (T n) . margin (B n)
margin (X n) = margin (L n) . margin (R n)
margin (XY x y) = margin (X x) . margin (Y y)
margin (TRBL t r b l) =
  margin (T t) . margin (R r) . margin (B b) . margin (L l)
margin (T x) = utility ("mt" -. x) ["margin-top" :. style x]
margin (R x) = utility ("mr" -. x) ["margin-right" :. style x]
margin (B x) = utility ("mb" -. x) ["margin-bottom" :. style x]
margin (L x) = utility ("ml" -. x) ["margin-left" :. style x]
margin (TR t r) = margin (TRBL t r 0 0)
margin (TL t l) = margin (TRBL t 0 0 l)
margin (BR b r) = margin (TRBL 0 r b 0)
margin (BL b l) = margin (TRBL 0 0 b l)


{- | Add a drop shadow to an element

> input (shadow Inner) "Inset Shadow"
> button (shadow ()) "Click Me"
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


-- | Set the background color. See 'Web.View.Types.ToColor'
bg :: (ToColor clr, Styleable h) => clr -> CSS h -> CSS h
bg c = utility ("bg" -. colorName c) ["background-color" :. style (colorValue c)]


data BorderStyle
  = Solid
  | Dashed
  deriving (Show, ToStyle, ToClassName)


border :: (Styleable h) => Sides PxRem -> CSS h -> CSS h
border s = borderWidth s . borderStyle Solid


borderStyle :: (Styleable h) => BorderStyle -> CSS h -> CSS h
borderStyle s = utility ("brds" -. s) ["border-style" :. style s]


-- | Round the corners of the element
rounded :: (Styleable h) => Length -> CSS h -> CSS h
rounded n = utility ("rnd" -. n) ["border-radius" :. style n]


{- | Set a border around the element

> el (border 1) "all sides"
> el (border (X 1)) "only left and right"
-}
borderWidth :: (Styleable h) => Sides PxRem -> CSS h -> CSS h
borderWidth (All n) =
  utility ("brd" -. n) ["border-width" :. style n]
borderWidth (Y n) = borderWidth (T n) . borderWidth (B n)
borderWidth (X n) = borderWidth (L n) . borderWidth (R n)
borderWidth (XY x y) = borderWidth (X x) . borderWidth (Y y)
borderWidth (TRBL t r b l) =
  borderWidth (T t) . borderWidth (R r) . borderWidth (B b) . borderWidth (L l)
borderWidth (T x) = utility ("brdt" -. x) ["border-top-width" :. style x]
borderWidth (R x) = utility ("brdt" -. x) ["border-right-width" :. style x]
borderWidth (B x) = utility ("brdt" -. x) ["border-bottom-width" :. style x]
borderWidth (L x) = utility ("brdt" -. x) ["border-left-width" :. style x]
borderWidth (TR t r) = borderWidth (TRBL t r 0 0)
borderWidth (TL t l) = borderWidth (TRBL t 0 0 l)
borderWidth (BR b r) = borderWidth (TRBL 0 r b 0)
borderWidth (BL b l) = borderWidth (TRBL 0 0 b l)


-- | Set a border color. See 'Web.View.Types.ToColor'
borderColor :: (ToColor clr, Styleable h) => clr -> CSS h -> CSS h
borderColor c =
  utility ("brdc" -. colorName c) ["border-color" :. style (colorValue c)]


opacity :: (Styleable h) => Float -> CSS h -> CSS h
opacity n =
  utility ("opacity" -. n) ["opacity" :. style n]
