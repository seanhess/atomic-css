{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.ByteString.Lazy (fromStrict)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import Web.Atomic


main :: IO ()
main = do
  putStrLn "Starting on http://localhost:3010/"
  Warp.run 3010 app


nav :: Html () -> Html ()
nav = tag "nav"


button :: Html () -> Html ()
button = tag "button"


input :: Html ()
input = tag "button" none


placeholder :: (Attributable h) => AttValue -> Attributes h -> Attributes h
placeholder = att "placeholder"


autofocus :: (Attributable h) => Attributes h -> Attributes h
autofocus = att "autofocus" ""


buttons :: Html ()
buttons = col ~ gap 10 . pad 20 $ do
  el ~ bold . fontSize 32 $ "My page"
  el ~ hover bold $ "hover"

  row ~ gap 10 $ do
    button ~ btn Primary $ "Do Something"
    button ~ btn Secondary $ "Cancel"

  button' Secondary ~ width 100 $ "Another Example"
 where
  -- Make style functions to encourage reuse
  btn c = bg c . hover (bg (light c)) . color White . rounded 3 . pad 15
  light Primary = PrimaryLight
  light Secondary = SecondaryLight
  light _ = Gray

  -- alternatively, we can make View functions
  button' c = button ~ btn c


inputs :: Html ()
inputs = do
  col ~ grow . pad 20 . gap 10 $ do
    el ~ bold $ "INPUT"
    input @ placeholder "Not Focused" ~ border 1 . pad 10 . bg White
    input @ placeholder "Should Focus" @ autofocus ~ border 1 . pad 10 . bg White


responsive :: Html ()
responsive = do
  nav ~ pad 20 . gap 10 . bg Primary . color White . menu $ do
    el ~ bold $ "MENU"
    el "One"
    el "Two"
    el "Three"

  col ~ content . grow . pad 20 . gap 20 . bg White $ do
    el ~ bold . fontSize 24 $ "Make the window smaller"
    el "This demonstrates how to create a responsive design. Resize the window under 800px wide and the nav bar will switch to a top bar"

    col ~ color Gray . gap 20 $ do
      el $ text lorem
      el $ text lorem
      el $ text lorem
      el $ text lorem
      el $ text lorem
      el $ text lorem
      el $ text lorem
 where
  menuWidth = 250
  menuHeight = 70

  menu = big sidebar . small topbar
  sidebar = width menuWidth . position Fixed . flexCol . top 0 . bottom 0 . left 0
  topbar = height menuHeight . position Fixed . flexRow . top 0 . left 0 . right 0

  content = big (margin (L menuWidth)) . small (margin (T menuHeight))

  big :: (Styleable c) => (CSS c -> CSS c) -> (CSS c -> CSS c)
  big = media (MinWidth 800)

  small :: (Styleable c) => (CSS c -> CSS c) -> (CSS c -> CSS c)
  small = media (MaxWidth 800)


holygrail :: Html ()
holygrail = col ~ grow $ do
  row ~ bg Primary $ "Top Bar"
  row ~ grow $ do
    col ~ bg Secondary $ "Left Sidebar"
    col ~ grow $ do
      text "Content Upper Left"
      space
      row $ do
        space
        text "Content Bottom Right"
    col ~ bg Secondary $ "Right Sidebar"
  row ~ bg Primary $ "Bottom Bar"


tooltips :: Html ()
tooltips = do
  col ~ pad 10 . gap 10 . width 300 $ do
    el ~ bold $ "CSS ONLY TOOLTIPS"
    mapM_ viewItemRow ["One", "Two", "Three", "Four", "Five", "Six"]
 where
  viewItemRow item = do
    col ~ stack . showTooltips . hover (color red) . pointer $ do
      el ~ border 1 . bg White $ text item
      el ~ cls "tooltip" . popup (TR 10 10) . zIndex 1 . visibility Hidden $ do
        col ~ border 2 . gap 5 . bg White . pad 5 $ do
          el ~ bold $ "ITEM DETAILS"
          el $ text item
          el "details lorem blah blah blah"

  showTooltips =
    css
      "tooltips"
      ".tooltips:hover > .tooltip"
      (declarations $ visibility Visible)

  red = HexColor "#F00"


stacks :: Html ()
stacks = col ~ grow $ do
  row ~ bg Primary . bold . pad 10 . color White $ "Stacks"
  col ~ pad 10 . gap 10 $ do
    el "Stacks put contents on top of each other"
    col ~ stack . border 1 $ do
      el ~ bg Light . pad 10 $ "In the background"
      col ~ pad 10 $ do
        row $ do
          space
          el ~ bg SecondaryLight . grow . pad 5 $ "Above"
      el ~ pad (XY 15 5) $ do
        row $ do
          space
          el ~ bg Primary . pad 10 . color White $ "Max Above@"

    el "We can collapse items in a stack so they don't affect the width"
    col ~ stack . bg Light . pad 10 $ do
      col $ do
        row ~ gap 5 $ do
          el "Some"
          el "Stuff"
          el "Here"
      col ~ popup (BR 0 0) . pad 10 . bg SecondaryLight $ do
        el "One"
        el "Two"
        el "Three"
        el "Four"

    col ~ stack . border 1 $ do
      col ~ bg Light $ "Background"
      col ~ bg SecondaryLight . opacity 0.8 . popup (X 50) $ do
        el "HMM"
        el "OK"
      row ~ bg Warning . opacity 0.8 $ do
        space
        el "Overlay"

    el ~ bold $ "Example Popup Search"
    el ~ stack . border 1 $ do
      row ~ bg Light . pad 10 $ "This is a search bar"
      col ~ popup (TRBL 43 5 5 5) . border 1 $ do
        col ~ bg SecondaryLight . pad (L 50) . pad (R 50) $ do
          el ~ hover (bg White) . pointer $ "I am a popup"
          el "I am a popup"
          el "I am a popup"
          el "I am a popup"

    col ~ gap 10 $ do
      el "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "

    col ~ border 1 . popup (TR 5 5) $ "I AM AN ELEMENT"


texts :: Html ()
texts = col ~ gap 10 . pad 20 $ do
  el ~ bg Warning . bg Error $ "Error"
  -- el ~ bg Error . bg Warning ~ if True then bold else id $ "Warning"

  el ~ pad 10 $ do
    el ~ descendentOf "htmx-request" flexRow . display None $ "Loading..."
    el ~ descendentOf "htmx-request" (display None) . flexRow $ "Normal Content"

  el ~ italic $ "Italic Text"
  el ~ underline $ "Underline Text"
  el ~ bold $ "Bold Text"

  -- ol [] $ do
  --   let nums = list Decimal
  --   li nums "first"
  --   li nums "second"
  --   li nums "third"
  --
  -- ul [] $ do
  --   li (list Disc) "first"
  --   li (list Disc) "second"
  --   li (list None) "third"

  el ~ bold $ "flexWrap"
  row ~ gap 5 . width 200 . flexWrap WrapReverse $ do
    el ~ border 1 . pad 5 $ "one"
    el ~ border 1 . pad 5 $ "two"
    el ~ border 1 . pad 5 $ "three"
    el ~ border 1 . pad 5 $ "four"
    el ~ border 1 . pad 5 $ "five"
    el ~ border 1 . pad 5 $ "six"
    el ~ border 1 . pad 5 $ "seven"
    el ~ border 1 . pad 5 $ "eight"
    el ~ border 1 . pad 5 $ "nine"

  el ~ bold $ "White Space: text wrap"
  el ~ border 1 . width 200 . whiteSpace NoWrap . overflow Hidden $ text lorem
  el ~ border 1 . width 200 . whiteSpace Wrap $ text lorem

  el ~ bold $ "css order"
  el ~ flexCol . flexRow $ do
    text "WOOT"
    text "BOOT"


lorem :: Text
lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."


longContent :: Html ()
longContent = do
  col ~ gap 10 . pad 10 $ do
    resultsTable $ replicate 100 "asdf"
 where
  resultsTable langs = do
    col ~ gap 15 $ do
      mapM_ languageRow langs
   where
    languageRow lang = do
      col ~ gap 5 $ do
        button ~ pad (XY 10 2) . border 1 . hover (bg Light) $ "Select"
        row $ do
          row $ do
            row $ do
              row $ do
                row $ do
                  tag "div" ~ bg Light . pad (XY 10 2) . fontSize 16 . textAlign AlignCenter $ text lang


-- rows = textAlign AlignCenter . border 1 . borderColor GrayLight

examples :: Html ()
examples = col ~ pad 20 . gap 15 $ do
  el ~ bold . fontSize 24 $ "Layout"
  link "buttons" "Buttons"
  link "responsive" "Responsive"
  link "holygrail" "Holy Grail"
  link "stacks" "Stacks"
  link "text" "Text"
  link "inputs" "Inputs"
  link "tooltips" "Tooltips"
  link "long-content" "Long Content"
 where
  link href = tag "a" @ att "href" href ~ color Primary


app :: Application
app req respond = do
  case pathInfo req of
    [] -> view examples
    ["buttons"] -> view buttons
    ["responsive"] -> view responsive
    ["holygrail"] -> view holygrail
    ["stacks"] -> view stacks
    ["text"] -> view texts
    ["inputs"] -> view inputs
    ["tooltips"] -> view tooltips
    ["long-content"] -> view longContent
    ["static", "reset.css"] -> reset
    _ -> notFound
 where
  html h =
    respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] h

  notFound =
    respond $ responseLBS status404 [("Content-Type", "text/plain; charset=utf-8")] "Not Found"

  view v =
    html $ document $ renderLazyByteString v

  document cnt =
    [i|<html>
      <head><link rel="stylesheet" type="text/css" href="static/reset.css"></link>
      <body>#{cnt}</body>
    </html>|]

  reset =
    respond $ responseLBS status200 [("Content-Type", "text/css; charset=utf-8")] (fromStrict cssResetEmbed)


data AppColor
  = White
  | Light
  | Gray
  | Dark
  | Success
  | Error
  | Warning
  | Primary
  | PrimaryLight
  | Secondary
  | SecondaryLight
  deriving (Show)


instance ToColor AppColor where
  colorValue White = "#FFF"
  colorValue Light = "#F2F2F3"
  colorValue Gray = "#888"
  colorValue Dark = "#2E3842" -- "#232C41"
  colorValue Primary = "#2C74BB"
  colorValue PrimaryLight = "#3281cf"
  colorValue Success = "#D5E6DE"
  colorValue Error = "#F3D8DA"
  colorValue Warning = "#FDF3D1"
  colorValue Secondary = "#5CADDB"
  colorValue SecondaryLight = "#6CBDEB"
