module Web.Atomic.Attributes
  ( Attributable (..)
  , class_
  , att
  , Name
  , AttValue
  , Attributes
  ) where

import Data.Map.Strict qualified as M
import Web.Atomic.Types


-- merge class names instead of replacing them, separating by spaces
-- this is no good!
-- the merging won't preserve this logic
class_ :: (Attributable h) => AttValue -> Attributes h -> Attributes h
class_ cnew (Attributes m) =
  Attributes $ M.insertWith (\a b -> a <> " " <> b) "class" cnew m
