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


-- TODO: update readme
-- TODO: decide on a tagline / synopsis and put it everywhere

{- $html
We also provide an Html Monad and combinator library with basic functions to generate html and add attributes with the `(@)` operator
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
'el' ~ 'bold' $ "Hello World"
@

This renders as the following HTML with embedded CSS definitions

> <style type='text/css'>
> .bold { font-weight:bold }
> </style>
>
> <div class='bold'>Hello World</div>

Instead of relying on the fickle cascade, factor and compose styles with the full power of Haskell functions!

> header = bold
> h1 = header . fontSize 32
> h2 = header . fontSize 24
> page = flexCol . gap 10 . pad 10
>
> example = el ~ page $ do
>   el ~ h1 $ "My Page"
>   el ~ h2 $ "Introduction"
>   el "lorem ipsum yada yada yada"
>   ...

This approach is inspired by Tailwindcss' [Utility Classes](https://tailwindcss.com/docs/styling-with-utility-classes)
-}
