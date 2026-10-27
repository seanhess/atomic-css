{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.ByteString.Lazy (fromStrict)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intersperse)
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import Web.Atomic
import Web.Atomic.Html


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

alignToBaseline :: Styleable h => CSS h -> CSS h
alignToBaseline = utility "align-to-baseline" ["align-items" :. "baseline"]

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

  el ~ bold $ "text baseline alignment"
  row ~ gap 20 $ do
    col $ do
      el "without baseline alignment"
      row ~ border 1 . gap 10 . pad 10 $ do
        el ~ fontSize 12 $ "small"
        el ~ fontSize 50 $ "big"
        el ~ fontSize 22 $ "middle"

    col $ do
      el "with baseline alignment"
      row ~ alignToBaseline . border 1 . gap 10 . pad 10 $ do
        el ~ fontSize 12 $ "small"
        el ~ fontSize 50 $ "big"
        el ~ fontSize 22 $ "middle"

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

  el ~ bold $ "css order"
  el ~ flexCol . flexRow $ do
    text "WOOT"
    text "BOOT"

  el ~ bold $ "text align"
  row  ~ gap 30 . grow $ do
    let c = text lorem
    tAlign c AlignLeft    "left"
    tAlign c AlignCenter  "center"
    tAlign c AlignRight   "right"
    tAlign c AlignJustify "justify"

  where
    tAlign c align t = col $ do
                            el t
                            el ~ pad 10 . textAlign align . border 1 $ c

lorem :: Text
lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

whiteSpaceText :: Html ()
whiteSpaceText = do
    col $ do
      el ~ bold $ "White Space: text wrap"

      row ~ gap 30 . pad 10 $ do
        -- inspired from https://css-tricks.com/almanac/properties/w/whitespace/
        whiteSpaces Wrap "Wrap"
        whiteSpaces PreWrap "PreWrap"
        whiteSpaces PreLine "PreLine"
        whiteSpaces BreakSpaces "BreakSpaces"
        whiteSpaces NoWrap "NoWrap"
        whiteSpaces Pre "Pre"
  where
    wSpace c align wrap t = col $ do
                              el t
                              el ~ pad 10 . textAlign align . whiteSpace wrap . border 1 . overflow Hidden $ c
    intersperseWithSpace sp ts = Html () $ intersperse sp $ map Text ts
    whiteSpaces algn algnT = col ~ width 100 . grow $ do
                           el algnT
                           col ~ pad 20 . gap 20 . border 1 $ do
                             let t sp = intersperseWithSpace (Raw sp) $ T.words lorem
                             wSpace (t " ") AlignLeft algn "[space]"
                             wSpace (t "&thinsp;") AlignLeft algn "&thinsp;"
                             wSpace (t "&nbsp;") AlignLeft algn "&nbsp;"
                             wSpace (t "&emsp;") AlignLeft algn "&emsp;"
                             wSpace (t "&zwj;") AlignLeft algn "&zwj;"
                             wSpace (t "&zwj;") AlignLeft algn "&zwj;"
                             wSpace (t "&zwnj;") AlignLeft algn "&zwnj;"

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

layout :: Html ()
layout = col ~ grow $ do
  row  ~ pad 10 . gap 30 $ do
    col $ do
      el ~ bold $ "spaces around & between elements"
      row ~ gap 30 $ do
        col ~ pad 10 . grow $ do
          el "pad 20"
          row ~ flexCol . border 1 . pad 20 . bg Warning $ do
            el ~ border 1 . bg White $ "one"
            el ~ border 1 . bg White $ "two"
            el ~ border 1 . bg White $ "three"
        col ~ pad 10 . grow $ do
          el "gap 20"
          row ~ flexCol . border 1 . gap 20 . bg Warning $ do
            el ~ border 1 . bg White $ "one"
            el ~ border 1 . bg White $ "two"
            el ~ border 1 . bg White $ "three"
        col ~ pad 10 . grow $ do
          el "pad 20 . gap 20"
          row ~ flexCol . border 1 . pad 20 . gap 20 . bg Warning $ do
            el ~ border 1 . bg White $ "one"
            el ~ border 1 . bg White $ "two"
            el ~ border 1 . bg White $ "three"
  -- inspired from: https://css-tricks.com/snippets/css/a-guide-to-flexbox/
  col ~ pad 10 . gap 30 . grow $ do
    el ~ bold $ "flexWrap"
    row  ~ pad 10 . gap 30 $ do
      col $ do
        space
        el "flex-direction: row"
        flexDir row
      col $ do
        space
        el "flex-direction: column"
        flexDir col
      col $ do
        space
        el "flex-wrap: wrap"
        flexWrap' Wrap
      col $ do
        space
        el "flex-wrap: wrap-reverse"
        flexWrap' WrapReverse
    row  ~ pad 10 . gap 30 $ do
      col ~ grow $ do
        el "flex-grow"
        flexGrow
    col ~ pad 10 . gap 30 . grow $ do
      el ~ bold $ "vertical positioning by using `space`"
      row  ~ pad 10 . gap 30 . grow $ do
        col ~ pad 10 $ do
          el "flex-start"
          flexStart
        col ~ pad 10 . grow $ do
          el "flex-center"
          flexCenter
        col ~ pad 10 . grow $ do
          el "flex-end"
          flexEnd
        col ~ pad 10 . grow $ do
          el "space-between"
          spaceBetween
        col ~ pad 10 . grow $ do
          el "space-around"
          spaceAround
  where
    box x = el ~ border 1 . pad 10 $ x
    flexDir dir = do
      dir ~ border 1 . pad 30 $ do
        do
          box "one"
          box "two"
          box "three"
    flexWrap' wrap = do
      row ~ flexWrap wrap . border 1 . pad 10 $ do
          box "one"
          box "two"
          box "three"
          box "four"
          box "five"
          box "six"
          box "seven"
          box "eight"
          box "nine"
          box "ten"
          box "eleven"
          box "twelve"
    flexGrow = do
      row ~ border 1 . pad 30 $ do
        do
          box "one"
          box ~ grow $ "two"
          box "three"
    r1 = row $ do
          box ~ grow $ "one"
          box ~ grow $ "two"
          box "three"
    r2 = row $ do
          box "four"
          box "five"
          box ~ grow $ "six"
          box "seven"
    r3 = row $ do
          box "eight"
          box "nine"
    spaceYellow = el ~ bg Warning . grow $ space
    flexStart = box ~ grow $ do
      r1 >> r2 >> r3
    flexCenter = col ~ border 1 . pad 10 . grow $ do
         spaceYellow >> r1 >> r2 >> r3 >> spaceYellow
    flexEnd = col ~ border 1 . pad 10 . grow $ do
         spaceYellow >> r1 >> r2 >> r3
    spaceBetween = col ~ border 1 . pad 10 . grow $ do
         r1 >> spaceYellow >> r2 >> spaceYellow >> r3
    spaceAround = col ~ border 1 . pad 10 . grow $ do
         spaceYellow >> r1 >> spaceYellow >> r2 >> spaceYellow >> r3 >> spaceYellow

-- rows = textAlign AlignCenter . border 1 . borderColor GrayLight

examples :: Html ()
examples = col ~ pad 20 . gap 15 $ do
  el ~ bold . fontSize 24 $ "Layout"
  link "buttons" "Buttons"
  link "responsive" "Responsive"
  link "holygrail" "Holy Grail"
  link "stacks" "Stacks"
  link "text" "Text"
  link "whitespace-text" "Whitespace Text"
  link "inputs" "Inputs"
  link "tooltips" "Tooltips"
  link "long-content" "Long Content"
  link "layout" "Layout"
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
    ["whitespace-text"] -> view whiteSpaceText
    ["inputs"] -> view inputs
    ["tooltips"] -> view tooltips
    ["long-content"] -> view longContent
    ["layout"] -> view layout
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
