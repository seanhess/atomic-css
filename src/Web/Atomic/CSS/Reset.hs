{-# LANGUAGE TemplateHaskell #-}

module Web.Atomic.CSS.Reset where

import Data.ByteString
import Data.FileEmbed
import Data.Text


{- | Default CSS to remove unintuitive default styles. This or 'cssResetLink' is required for utilities to work as expected, especially the box model.

> import Data.String.Interpolate (i)
>
> toDocument :: ByteString -> ByteString
> toDocument cnt =
>   [i|<html>
>     <head>
>       <style type="text/css">#{cssResetEmbed}</style>
>     </head>
>     <body>#{cnt}</body>
>   </html>|]
-}
cssResetEmbed :: ByteString
cssResetEmbed = $(embedFile "embed/preflight.css")


{- | Alternatively, the reset is available on a CDN

> import Data.String.Interpolate (i)
>
> toDocument :: ByteString -> ByteString
> toDocument cnt =
>   [i|<html>
>     <head>
>       <link rel="stylesheet" href="#{cssResetUrl}">
>     </head>
>     <body>#{cnt}</body>
>   </html>|]
-}
cssResetUrl :: Text
cssResetUrl = "<link rel=\"stylesheet\" href=\"https://unpkg.com/tailwindcss@3.3.3/src/css/preflight.css\"/>"
