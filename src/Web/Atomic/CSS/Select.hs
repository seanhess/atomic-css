module Web.Atomic.CSS.Select where

import Web.Atomic.Types


{- | Apply when hovering over an element

> el ~ bg Primary . hover (bg PrimaryLight) $ "Hover"
-}
hover :: (Styleable h) => (CSS h -> CSS h) -> CSS h -> CSS h
hover = pseudo "hover"


-- | Apply when the mouse is pressed down on an element
active :: (Styleable h) => (CSS h -> CSS h) -> CSS h -> CSS h
active = pseudo "active"


-- | Apply to even-numbered children
even :: (Styleable h) => (CSS h -> CSS h) -> CSS h -> CSS h
even = pseudo $ Pseudo "even" ":nth-child(even)"


-- | Apply to odd-numbered children
odd :: (Styleable h) => (CSS h -> CSS h) -> CSS h -> CSS h
odd = pseudo $ Pseudo "odd" ":nth-child(odd)"


pseudo :: forall h. (Styleable h) => Pseudo -> (CSS h -> CSS h) -> CSS h -> CSS h
pseudo p f ss =
  mapRules (addPseudo p) (f mempty) <> ss


{- | Apply when the Media matches the current window. This allows for responsive designs

> el ~ width 100 . media (MinWidth 800) (width 400) $ do
>   "Big if window > 800"
-}
media :: (Styleable h) => Media -> (CSS h -> CSS h) -> CSS h -> CSS h
media m f ss =
  mapRules (addMedia m) (f mempty) <> ss


addPseudo :: Pseudo -> Rule -> Rule
addPseudo p r = r{selector = r.selector <> GeneratedRule (addClassState p) (<> p.suffix)}


addMedia :: Media -> Rule -> Rule
addMedia m r =
  r
    { media = m : r.media
    , selector = r.selector <> GeneratedRule (addClassState m) id
    }


{- | Apply when this element is contained somewhere another element with the given class

> el ~ descendentOf "htmx-request" bold $ "Only bold when htmx is making a request"
-}
descendentOf :: (Styleable h) => ClassName -> (CSS h -> CSS h) -> CSS h -> CSS h
descendentOf c f ss =
  mapRules (addAncestor c) (f mempty) <> ss


addAncestor :: ClassName -> Rule -> Rule
addAncestor cn r = r{selector = r.selector <> GeneratedRule (addClassState cn) (\s -> selector cn <> " " <> s)}
