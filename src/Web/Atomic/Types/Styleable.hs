module Web.Atomic.Types.Styleable where

import Web.Atomic.Types.ClassName
import Web.Atomic.Types.Rule as Rule
import Web.Atomic.Types.Selector
import Web.Atomic.Types.Style


class Styleable h where
  -- | Apply a CSS utility to some html
  --
  -- > el ~ bold . border 1 $ "styled"
  -- > el "styled" ~ bold . border 1
  -- > el "not styled"
  (~) :: h -> (CSS h -> CSS h) -> h
  h ~ f =
    flip modCSS h $ \rs ->
      let CSS new = f $ CSS rs
       in uniqueRules new


  modCSS :: ([Rule] -> [Rule]) -> h -> h


infixl 5 ~


instance {-# OVERLAPPABLE #-} (Styleable a, Styleable b) => Styleable (a -> b) where
  (~) :: (a -> b) -> (CSS (a -> b) -> CSS (a -> b)) -> (a -> b)
  hh ~ f = \content ->
    hh content ~ \(CSS m) ->
      let CSS m2 = f $ CSS m
       in CSS m2


  modCSS r hh content =
    modCSS r $ hh content


instance Styleable [Rule] where
  modCSS f = f


instance Styleable (CSS h) where
  modCSS f (CSS rs) = CSS $ f rs


newtype CSS h = CSS {rules :: [Rule]}
  deriving newtype (Monoid, Semigroup)


mapRules :: (Rule -> Rule) -> CSS a -> CSS a
mapRules f (CSS rs) = CSS $ fmap f rs


{- | Create an atomic CSS utility. These are classes that set a single property, allowing you to compose styles like functions

@
bold :: 'Styleable' h => 'CSS' h -> 'CSS' h
bold = utility "bold" ["font-weight" :. "bold"]

pad :: 'Styleable' h => 'PxRem' -> 'CSS' h -> 'CSS' h
pad px = utility ("pad" -. px) ["padding" :. 'style' px]

example = el ~ bold . pad 10 $ "Padded and bold"
@
-}
utility :: (Styleable h) => ClassName -> [Declaration] -> CSS h -> CSS h
utility cn ds (CSS rs) =
  CSS $ rule cn ds : rs


{- | Apply a class name with no styles. Useful for external CSS

> el ~ cls "parent" $ do
>   el ~ cls "item" $ "one"
>   el ~ cls "item" $ "two"
-}
cls :: (Styleable h) => ClassName -> CSS h -> CSS h
cls cn (CSS rs) =
  CSS $ Rule.fromClass cn : rs


{- | Embed CSS with a custom selector and apply it to an element. Modifiers like 'hover' will ignore this

> listItems =
>   css
>     "list"
>     ".list > .item"
>     [ "display" :. "list-item"
>     , "list-style" :. "square"
>     ]
>
> example = do
>   el ~ listItems $ do
>     el ~ cls "item" $ "one"
>     el ~ cls "item" $ "two"
>     el ~ cls "item" $ "three"
-}
css :: (Styleable h) => ClassName -> Selector -> [Declaration] -> CSS h -> CSS h
css cn sel ds (CSS rs) =
  CSS $ Rule cn (CustomRule sel) mempty ds : rs


-- | Get all the rules for combined utilities
rules :: (CSS [Rule] -> CSS [Rule]) -> [Rule]
rules f =
  let CSS rs = f mempty
   in rs


-- | Get all the declarations for a utility or combination of them
declarations :: (CSS [Rule] -> CSS [Rule]) -> [Declaration]
declarations f =
  mconcat $ fmap (.properties) (rules f)
