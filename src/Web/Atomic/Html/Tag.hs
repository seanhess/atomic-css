{- |
Module:      Web.Atomic.Html.Tag
Copyright:   (c) 2023 Sean Hess
License:     BSD3
Maintainer:  Sean Hess <seanhess@gmail.com>
Stability:   experimental
Portability: portable

We can intuitively create layouts by combining 'row', 'col', 'space', and 'stack'

@
holygrail = do
  col ~ grow $ do
    row do
      el "Top Bar"
      space
      el "Login Button"
    row ~ grow $ do
      col "Left Sidebar"
      col ~ grow $ do
        el "Main Content"
      col "Right Sidebar"
    row "Bottom Bar"
@
-}
module Web.Atomic.Html.Tag where

import Web.Atomic.CSS
import Web.Atomic.Html


{- |

@
col = 'el' ~ 'flexCol'
@
-}
col :: Html () -> Html ()
col = el ~ flexCol


{- |

@
col = 'el' ~ 'flexRow'
@
-}
row :: Html () -> Html ()
row = el ~ flexRow


{- |

@
col = 'el' ~ 'grow'
@
-}
space :: Html ()
space = el ~ grow $ none
