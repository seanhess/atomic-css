{-# LANGUAGE TemplateHaskell #-}

module Web.Atomic.CSS.Reset where

import Data.ByteString
import Data.FileEmbed


{- | Default CSS to remove unintuitive default styles. This is required for utilities to work as expected

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
cssResetEmbed = $(embedFile "embed/reset.css")
