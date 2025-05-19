{- |
Module:      Web.Atomic
Copyright:   (c) 2023 Sean Hess
License:     BSD3
Maintainer:  Sean Hess <seanhess@gmail.com>
Stability:   experimental
Portability: portable

Type-safe Atomic CSS with intuitive layouts and composable css utility classes. Inspired by Tailwindcss and Elm-UI
-}
module Web.Atomic
  ( -- * How to use this library
    -- $use
    module Web.Atomic.Types

    -- ** Atomic CSS
    -- $css
  , module Web.Atomic.CSS

    -- ** Html Monad
    -- $html
  , Html
  , el
  , tag
  , none
  , raw
  , renderText
  , renderLazyText
  , renderLazyByteString
  ) where

import Web.Atomic.CSS
import Web.Atomic.Html
import Web.Atomic.Render
import Web.Atomic.Types


{- $html
We also provide a useful Html Monad and combinator library (no Html5 tags or attributes are exported)
-}


{- $css
The main purpose of atomic-css is to provide CSS Utilities and the `(~)` operator to style HTML. These utilities can be used by any combinator library. See [Hyperbole](https://github.com/seanhess/hyperbole)

@
bold :: 'Styleable' h => 'CSS' h -> 'CSS' h
bold = utility "bold" ["font-weight" :. "bold"]

pad :: 'Styleable' h => 'PxRem' -> 'CSS' h -> 'CSS' h
pad px = utility ("pad" -. px) ["padding" :. 'style' px]

example = el ~ bold . pad 10 $ "Padded and bold"
@

See Web.Atomic.CSS for a full list of utilities provided by this library
-}


{- $use

Create stylish html using composable haskell functions:

@
import Web.Atomic

example :: 'Html' ()
example = 'el' ~ 'flexCol' . 'gap' 10 $ do
 'el' ~ 'bold' . 'fontSize' 32 $ "My page"
 'el' ~ 'border' 1 $ "Hello!"
@

This renders as the following HTML with embedded CSS definitions

> <style type='text/css'>
> .bold { font-weight:bold }
> .brd-1 { border:1px; border-style:solid }
> .col { display:flex; flex-direction:column }
> .fs-32 { font-size:2.0rem }
> .gap-10 { gap:0.625rem }
> </style>
>
> <div class='col gap-10'>
>   <div class='bold fs-32'>My page</div>
>   <button class='brd-1'>Click Me</button>
> </div>

Factor your styles with the full power of Haskell functions, instead of relying on the cascade

> header = bold
> h1 = header . fontSize 32
> h2 = header . fontSize 24
> page = gap 10
>
> example = col ~ page $ do
>   el ~ h1 $ "My Page"
>   el "some content"
>   ...

This approach is inspired by Tailwindcss' [Utility Classes](https://tailwindcss.com/docs/styling-with-utility-classes)
-}
