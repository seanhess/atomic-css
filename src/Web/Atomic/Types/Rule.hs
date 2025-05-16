{-# LANGUAGE LambdaCase #-}

module Web.Atomic.Types.Rule where

import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isNothing)
import Data.String (IsString (..))
import Web.Atomic.Types.ClassName
import Web.Atomic.Types.Selector
import Web.Atomic.Types.Style


-- Rule: CSS Utility Classes  ------------------------------------------------

data Rule = Rule
  { className :: ClassName
  , selector :: RuleSelector
  , media :: [Media]
  , properties :: [Declaration]
  }
instance Eq Rule where
  r1 == r2 = ruleSelector r1 == ruleSelector r2
instance Ord (Rule) where
  r1 <= r2 = ruleSelector r1 <= ruleSelector r2
instance IsString Rule where
  fromString s = fromClass (fromString s)


data RuleSelector
  = CustomRule Selector
  | GeneratedRule (ClassName -> ClassName) (Selector -> Selector)
instance Semigroup RuleSelector where
  CustomRule s1 <> CustomRule s2 = CustomRule $ s1 <> s2
  GeneratedRule c1 s1 <> GeneratedRule c2 s2 = GeneratedRule (c2 . c1) (s2 . s1)
  -- ignore FromClass if CustomRule is set!
  CustomRule c <> _ = CustomRule c
  _ <> CustomRule c = CustomRule c
instance Monoid RuleSelector where
  mempty = GeneratedRule id id


-- rule :: ClassName -> [Declaration] -> Rule
-- rule cn ds =
--   (Rule cn (selector cn) mempty ds)

-- | An empty rule that only adds the classname
fromClass :: ClassName -> Rule
fromClass cn = Rule cn mempty mempty mempty


rule :: ClassName -> [Declaration] -> Rule
rule cn ds = Rule cn mempty mempty ds


ruleMap :: [Rule] -> Map Selector Rule
ruleMap rs = foldl' (\m r -> M.insert (ruleSelector r) r m) M.empty rs


{- | Add a property to a class
addProp :: (ToStyleValue val) => Property -> val -> Rule -> Rule
addProp p v c =
  c{properties = Declaration p (toStyleValue v) : c.properties}
-}

-- mapSelector :: (Selector -> Selector) -> Rule -> Rule
-- mapSelector f c =
--   c
--     { selector = f c.selector
--     }

mapClassName :: (ClassName -> ClassName) -> Rule -> Rule
mapClassName f c =
  c
    { className = f c.className
    }


uniqueRules :: [Rule] -> [Rule]
uniqueRules [] = []
uniqueRules (r : rs) =
  r : (replaceRules r $ uniqueRules rs)


replaceRules :: Rule -> [Rule] -> [Rule]
replaceRules rnew rs =
  -- OVERRIDE RULES
  -- 1. if ANY property is set again, delete entire previous rule
  -- 2. if "manual" mode is set, pass it through!
  -- 3. if pseudo, media, etc, changes when these rules apply
  let ps = ruleProperties rnew
   in filter (not . matchesRule ps) rs
 where
  matchesRule ps r =
    (hasAnyProperty ps r || rnew.className == r.className)
      && ruleClassNameF rnew.selector "" == ruleClassNameF r.selector ""
      && isNothing (ruleCustomSelector rnew)
      && isNothing (ruleCustomSelector r)


hasAnyProperty :: [Property] -> Rule -> Bool
hasAnyProperty ps r = any hasProperty ps
 where
  hasProperty :: Property -> Bool
  hasProperty p = p `elem` ruleProperties r


ruleProperties :: Rule -> [Property]
ruleProperties r =
  fmap (\(p :. _) -> p) r.properties


lookupRule :: ClassName -> [Rule] -> Maybe Rule
lookupRule c = L.find (\r -> r.className == c)


ruleClassName :: Rule -> ClassName
ruleClassName r =
  ruleClassNameF r.selector r.className


ruleClassNameF :: RuleSelector -> ClassName -> ClassName
ruleClassNameF rs =
  case rs of
    CustomRule _ -> id
    GeneratedRule f _ -> f


ruleSelector :: Rule -> Selector
ruleSelector r =
  ruleSelectorF r.selector $ selector $ ruleClassName r


ruleSelectorF :: RuleSelector -> Selector -> Selector
ruleSelectorF rs =
  case rs of
    CustomRule s -> const s
    GeneratedRule _ f -> f


-- where
--  pseudos = mconcat . fmap pseudoSuffix

-- rulePseudo :: Rule -> [Pseudo]
-- rulePseudo r =
--   case r.selector of
--     CustomRule _ -> []
--     FromClass ps _ -> ps

ruleCustomSelector :: Rule -> Maybe Selector
ruleCustomSelector r =
  case r.selector of
    CustomRule s -> Just s
    _ -> Nothing
