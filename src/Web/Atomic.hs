module Web.Atomic
  ( module Web.Atomic.CSS
  , module Web.Atomic.Types
  , module Web.Atomic.Html
  , module Web.Atomic.Render
  ) where

import Web.Atomic.CSS
import Web.Atomic.Html
import Web.Atomic.Render
import Web.Atomic.Types


{- $use

Create stylish html using composable haskell functions:

> import Web.Atomic
>
> example :: Html ()
> example = col ~ gap 10 $ do
>  el ~ bold . fontSize 32 $ "My page"
>  button ~ border 1 $ "Click Me"

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
