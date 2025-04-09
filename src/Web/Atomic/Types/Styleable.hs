module Web.Atomic.Types.Styleable where

import Web.Atomic.Types.ClassName
import Web.Atomic.Types.Rule as Rule
import Web.Atomic.Types.Selector
import Web.Atomic.Types.Style


-- CHECKLIST REQUIREMENTS
-- DONE: hover only works on utilities
-- DONE: changing a utility overrides the previous one
-- DONE: can add custom css
-- DONE: utilities can set multiple properties
-- DONE: if you override ANY property in a utility it is removed
-- DONE: don't override different pseudo states

class Styleable h where
  (~) :: h -> (CSS h -> CSS h) -> h
  h ~ f =
    let new = runCSS f
     in modCSS (uniqueRules . (new <>)) h


  modCSS :: ([Rule] -> [Rule]) -> h -> h


infixl 5 ~


instance {-# OVERLAPPABLE #-} (Styleable a, Styleable b) => Styleable (a -> b) where
  (~) :: (a -> b) -> (CSS (a -> b) -> CSS (a -> b)) -> (a -> b)
  hh ~ f = \content ->
    hh content ~ \(CSS m) ->
      let CSS m2 = f $ CSS m
       in CSS m2


  modCSS r hh = \content ->
    modCSS r $ hh content


instance Styleable [Rule] where
  modCSS f rs = f rs


instance Styleable (CSS h) where
  modCSS f (CSS rs) = CSS $ f rs


newtype CSS h = CSS {rules :: [Rule]}
  deriving newtype (Monoid, Semigroup)


runCSS :: (CSS h -> CSS h) -> [Rule]
runCSS f =
  let CSS rs = f mempty
   in rs


mapRules :: (Rule -> Rule) -> CSS a -> CSS a
mapRules f (CSS rs) = CSS $ fmap f rs


cls :: (Styleable h) => ClassName -> CSS h -> CSS h
cls cn (CSS rs) =
  CSS $ Rule.fromClass cn : rs


-- Custom CSS
css :: (Styleable h) => ClassName -> Selector -> [Declaration] -> CSS h -> CSS h
css cn sel ds (CSS rs) =
  CSS $ Rule cn (CustomRule sel) mempty ds : rs


utility :: (ToStyleValue s, Styleable h) => ClassName -> Property -> s -> CSS h -> CSS h
utility cn pn a =
  utility' cn [Declaration pn (toStyleValue a)]


utility' :: (Styleable h) => ClassName -> [Declaration] -> CSS h -> CSS h
utility' cn ds (CSS rs) =
  CSS $ rule cn ds : rs
