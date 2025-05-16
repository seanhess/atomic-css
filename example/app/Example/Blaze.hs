{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Example.Blaze where

import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.List qualified as L
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Effectful
import Effectful.State.Static.Local
import Text.Blaze.Html (Html)
import Text.Blaze.Html4.Strict qualified as H
import Text.Blaze.Html4.Strict.Attributes as HA hiding (title)
import Text.Blaze.Internal (Attributable (..), ChoiceString (..), MarkupM (..), StaticString (..))
import Text.Blaze.Renderer.Utf8
import Web.Atomic.CSS
import Web.Atomic.Render
import Web.Atomic.Types hiding (Attributable)
import Prelude hiding (div, head, id)


test :: IO ()
test = do
  let (_, h2) = execHtml simple
  putStrLn $ BLC.unpack $ renderMarkup h2
  putStrLn "------------------------------"

  let (rs, h) = execHtml page1

  putStrLn $ unpack $ renderLines $ cssRulesLines $ ruleMap rs
  putStrLn ""
  putStrLn $ BLC.unpack $ renderMarkup h


newtype Fusion a = Fusion {eff :: Eff '[State Html, State [Rule]] a}
  deriving newtype (Functor, Applicative, Monad)


simple :: Fusion ()
simple = do
  head ~ pad 10 ~ pad 5 $ pure ()


page1 :: Fusion ()
page1 = do
  html $ do
    head ~ pad 6 . pad 8 ~ pad 10 . pad 4 $ do
      title (text "Introduction page.")
      link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
    body ~ display Block ~ pad 8 $ do
      div ! id "header" ~ bold . pad 5 ~ pad 10 ~ display Flex $ text "Syntax"
      p $ text "This is an example of BlazeMarkup syntax."
      ul $ mapM_ (li . showHtml @Int) [1, 2, 3]


html :: Fusion () -> Fusion ()
html = tag H.html
head :: Fusion () -> Fusion ()
head = tag H.head
body :: Fusion () -> Fusion ()
body = tag H.body
title :: Fusion () -> Fusion ()
title = tag H.title
link :: Fusion ()
link = tag (const H.link) (pure ())
div :: Fusion () -> Fusion ()
div = tag H.div
p :: Fusion () -> Fusion ()
p = tag H.p
ul :: Fusion () -> Fusion ()
ul = tag H.ul
li :: Fusion () -> Fusion ()
li = tag H.li


instance Attributable (Fusion ()) where
  Fusion eff ! at = Fusion $ do
    eff
    modify @Html $ \h -> h ! at


instance Attributable (Fusion () -> Fusion ()) where
  parent ! at = \child -> do
    parent child
    Fusion $ modify @Html $ \h -> h ! at


instance Styleable (Fusion ()) where
  modCSS f (Fusion eff) = Fusion $ do
    eff

    h <- get @Html
    rsold <- get @[Rule]

    let rsnew = f $ lookupRules (getClass h) (classMap rsold)

    put $ L.nub $ rsnew <> rsold
    put $ insertClass (fmap (.className) rsnew) h


getClass :: MarkupM () -> [ClassName]
getClass = \case
  -- merge
  AddAttribute (StaticString _ _ "class") _ (Text v) _ ->
    classesFromValue v
  -- forward
  AddAttribute _ _ _ h -> getClass h
  AddCustomAttribute _ _ h -> getClass h
  Append _ ma -> getClass ma
  -- ignore
  Comment _ _ -> []
  Empty _ -> []
  -- insert
  _ -> []


insertClass :: [ClassName] -> MarkupM () -> MarkupM ()
insertClass cs = \case
  -- replace any existing class attribute
  AddAttribute (StaticString _ _ "class") _ (Text _) h ->
    addClassAttribute h
  -- forward
  AddAttribute raw key val h -> AddAttribute raw key val (insertClass cs h)
  AddCustomAttribute c1 c2 h -> AddCustomAttribute c1 c2 (insertClass cs h)
  Append mb ma -> Append mb (insertClass cs ma)
  -- ignore
  Comment s a -> Comment s a
  Empty a -> Empty a
  -- insert
  h -> addClassAttribute h
 where
  addClassAttribute h =
    AddAttribute "class" " class=\"" (Text $ classAttValue cs) h


-- classRules :: Map ClassName Rule -> Text -> [Rule]
-- classRules m val =
--   lookupRules (classesFromValue val) m

classMap :: [Rule] -> Map ClassName Rule
classMap rs = M.fromList $ fmap (\r -> (r.className, r)) rs


classAttValue :: [ClassName] -> Text
classAttValue cns =
  mconcat $ L.intersperse " " $ fmap (.text) cns


classesFromValue :: Text -> [ClassName]
classesFromValue = fmap ClassName . T.splitOn " "


lookupRules :: [ClassName] -> Map ClassName Rule -> [Rule]
lookupRules cn m =
  mapMaybe (\c -> M.lookup c m) cn


tag :: (Html -> Html) -> Fusion () -> Fusion ()
tag tg cnt = do
  let (rs, inner) = execHtml cnt
  addHtml $ tg inner
  addRules rs
  pure ()


addHtml :: Html -> Fusion ()
addHtml h = Fusion $ do
  modify (>> h)


addRules :: [Rule] -> Fusion ()
addRules rs = Fusion $ do
  modify (rs <>)


-- el :: [Rule] -> Eff es Html -> Eff es Html
-- el rs = tag Html.div

text :: Text -> Fusion ()
text t = addHtml $ H.toMarkup t


execHtml :: Fusion () -> ([Rule], Html)
execHtml a = do
  let ewrite = execState @Html (pure ()) $ a.eff :: Eff '[State [Rule]] Html
  let (h, rs) = runPureEff $ runState @[Rule] [] ewrite
  -- collapse the class tag into one
  (rs, h)


-- in -- h' = h ! class_ (rulesToClass rs) :: Html

showHtml :: (Show a) => a -> Fusion ()
showHtml a =
  addHtml $ H.toMarkup $ show a

-- this is kind of bullshit!
-- collapseClasses :: [Rule] -> MarkupM a -> MarkupM a
-- collapseClasses allRules mrk = do
--   let (h', cs) = runPureEff $ runWriter @[String] $ collapseClasses' mrk
--   traceM $ "CC " <> show cs
--   let rs = uniqueRules $ mapMaybe (flip lookupRule allRules) (classNames cs)
--   let classes = classesAttValue $ fmap (.className) rs
--   case classes of
--     Nothing -> h'
--     Just av -> h' ! class_ (fromString $ unpack $ av)
--  where
--   classNames :: [String] -> [ClassName]
--   classNames ss = mconcat $ fmap (fmap (ClassName . pack) . words) ss
--
--   collapseClasses' :: MarkupM a -> Eff '[Writer [String]] (MarkupM a)
--   collapseClasses' = \case
--     AddAttribute (StaticString _ _ "class") _ (String val) inner -> do
--       traceM $ "@class " <> val
--       tell [val]
--       -- strip the attribute for now, keep collecting classes
--       collapseClasses' inner
--     AddAttribute (StaticString _ _ "class") _ val inner -> do
--       --   traceM $ "@class " <> show val
--       collapseClasses' inner
--     AddCustomAttribute a b inner -> do
--       traceM "@a"
--       h <- collapseClasses' inner
--       -- keep collecting classes
--       pure $ AddCustomAttribute a b h
--     Append a b -> do
--       traceM " >> "
--       -- collect classes separately
--       let ha = collapseClasses allRules a
--       let hb = collapseClasses allRules b
--       pure $ Append ha hb
--     -- Leaf, CustomLeaf, Content, Comment, Empty, Parent, CustomParent
--     -- we don't need to walk children, because we execHtml in `tag`
--     h -> pure h
