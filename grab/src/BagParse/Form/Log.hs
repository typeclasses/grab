module BagParse.Form.Log
  ( Log (..), contextualizeLog
  ) where

import BagParse.Form.Name

import Data.Bifunctor
import Data.Coerce (coerce)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as Text


--- Log ---

newtype Log a =
  Log
    (Map Name (Set a))


--- Log functions ---

contextualizeLog ::
    NamePart -> Log a -> Log a

contextualizeLog x =
    Log .
    Map.fromList .
    map (first (\(Name xs) -> Name (x : xs))) .
    Map.toList .
    (\(Log m) -> m)


--- Log monoid ---

instance Ord a => Semigroup (Log a) where
    (<>) = coerce multimapUnion

instance Ord a => Monoid (Log a) where
    mempty = coerce multimapEmpty

type Multimap a b = Map a (Set b)

multimapEmpty :: Multimap a b
multimapEmpty = Map.empty

multimapUnion :: (Ord a, Ord b) =>
    Multimap a b ->
    Multimap a b ->
    Multimap a b

multimapUnion = Map.unionWith (<>)


--- Eq and Ord instances ---

deriving stock instance Eq a => Eq (Log a)
