{-# LANGUAGE

    BlockArguments, DeriveFunctor, DerivingStrategies,
    GADTs, LambdaCase, ScopedTypeVariables,
    StandaloneDeriving, TypeApplications

#-}

module Control.Grab
  (
  -- * The main type
    Grab (..)

  -- * Type aliases
  , Simple, Dump, Result, Extract

  -- * Creating grabs
  , success, failure, extract, select, dump

  -- * Modifying grabs
  , discardResidue, run, (>->), mapLog

  -- * Deconstructing results
  , residue, log, desideratum

  ) where

import Control.Applicative (Applicative (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Either (Either (..))
import Data.Functor (Functor (..))
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Prelude ()


--- The main type ---

{- |

A 'Grab':

  1. Consumes some portion (none, part, or all) of its input
     __@bag@__;

  2. Returns a 'Result':

      * A __@residue@__ consisting of the unconsumed input;

      * Some monoidal __@log@__ e.g. a list of error messages;

      * Some __@desideratum@__ (the object of desire) produced from
        the consumed input, or @Nothing@ if the grab failed.

Specializations of this type:

  * If the bag and residue types are the same, the grab
    is a __'Simple'__ grab.

  * If the residue is @()@, the grab is a __'Dump'__; it
    dumps out the entire bag so there is nothing remaining.

  * If the bag is @()@, the grab is just a single fixed
    __'Result'__, which consists of the residue, log, and
    maybe the desideratum.

  * If both the bag and residue are @()@, the grab is
    just the __'Extract'__, which consists of the log and
    maybe the desideratum.

-}

newtype Grab bag residue log desideratum =
  Grab
    (bag -> (residue, log, Maybe desideratum))


--- Type aliases ---

{- |

A 'Simple' grab:

  1. Consumes some portion (none, part, or all) of its input
     __@bag@__;

  2. Returns a 'Result':

      * A modified __@bag@__ representing the unconsumed
        portion of the input;

      * Some monoidal __@log@__ e.g. a list of error messages;

      * Some __@desideratum@__ (the object of desire) produced from
        the consumed input, or @Nothing@ if the grab failed.

-}

type Simple bag log desideratum = Grab bag bag log desideratum

{- | A 'Dump':

  1. Consumes all of its input __@bag@__;

  2. Returns a 'Extract':

      * Some monoidal __@log@__ e.g. a list of error messages;

      * Some __@desideratum@__ (the object of desire) produced from
        the consumed input, or @Nothing@ if the grab failed.
-}

type Dump bag log desideratum = Grab bag () log desideratum

{- | The result of performing a 'Grab'. Consists of:

  * A __@residue@__ consisting of the unconsumed input;

  * Some monoidal __@log@__ e.g. a list of error messages;

  * Some __@desideratum@__ (the object of desire) produced from
    the consumed input, or @Nothing@ if the grab failed.
-}

type Result residue log desideratum = Grab () residue log desideratum

{- | What is produced by performing a 'Dump'. Consists of:

  * Some monoidal __@log@__ e.g. a list of error messages;

  * Some __@desideratum@__ (the object of desire) produced from
    the consumed input, or @Nothing@ if the grab failed.
-}

type Extract log desideratum = Grab () () log desideratum


--- Functor ---

deriving stock instance Functor (Grab bag residue log)


--- Applicative functor ---

instance (bag ~ residue, Monoid log) =>
  Applicative (Grab bag residue log) where
    pure = grabPure
    (<*>) = grabAp

grabPure :: Monoid log => a -> Grab bag bag log a
grabPure a = Grab \bag -> (bag, mempty, Just a)

grabAp :: Monoid log =>
    Grab bag bag log (x -> a) ->
    Grab bag bag log x ->
    Grab bag bag log a

grabAp (Grab pf) (Grab px) =
    Grab \bag ->
        let
            (bag',  log1, f) = pf bag
            (bag'', log2, x) = px bag'
        in
            (bag'', log1 <> log2, f <*> x)


--- Bifunctor ---

instance Bifunctor (Grab bag residue) where
    bimap = bimapGrab

bimapGrab :: forall bag residue log log' a a'.
    (log -> log') ->
    (a -> a') ->
    Grab bag residue log  a ->
    Grab bag residue log' a'

bimapGrab f g (Grab x) =
    Grab \bag ->
        let
            (bag', log, a) = x bag
        in
            (bag', f log, fmap @Maybe g a)


--- Creating grabs ---

success :: Monoid log => desideratum -> Extract log desideratum
success x = extract mempty (Just x)

failure :: log -> Extract log desideratum
failure x = extract x (Nothing)

extract :: log -> Maybe desideratum -> Extract log desideratum
extract x y = Grab \() -> ((), x, y)

select :: Monoid log => (bag -> (residue, selection)) ->
                        Grab bag residue log selection
select f = Grab \i -> let (r, s) = f i
                      in  (r, mempty, Just s)

dump :: (bag -> Extract log desideratum) -> Dump bag log desideratum
dump f = Grab \i -> let p = f i
                    in  ((), log p, desideratum p)


--- Modifying grabs ---

(>->) :: Semigroup log
    => Grab bag residue log x
    -> Grab x r log desideratum
        -- ^ The residue of this 'Grab' will be ignored,
        --   so it usually ought to be a 'Dump'.
    -> Grab bag residue log desideratum

(>->) (Grab f) (Grab g) =
    Grab \i ->
        let (x, y, z) = f i
        in  case z of
                Nothing -> (x, y, Nothing)
                Just a -> let (_, y', z') = g a
                          in  (x, y <> y', z')

run :: bag -> Grab bag residue log desideratum ->
              Result residue log desideratum
run x (Grab f) = Grab \() -> f x

discardResidue :: Grab bag residue log desideratum ->
                  Dump bag log desideratum
discardResidue (Grab f) =
    Grab \bag -> let (_, y, z) = f bag
                 in  ((), y, z)

mapLog :: (log -> log') -> Grab bag residue log  desideratum ->
                           Grab bag residue log' desideratum
mapLog f = first f


--- Deconstructing results ---

{- | The desired object, if one was successfully extracted from the bag.

This function can be used with both 'Result' and 'Extract'. -}

desideratum :: Result residue log desideratum -> Maybe desideratum
desideratum (Grab f) = let (_, _, x) = f () in x

{- | Any extra information produced during the grab, such as error messages.

This function can be used with both 'Result' and 'Extract'. -}

log :: Result residue log desideratum -> log
log (Grab f) = let (_, x, _) = f () in x

{- | The portion of the bag that was not consumed by the grab. -}

residue :: Result residue log desideratum -> residue
residue (Grab f) = let (x, _, _) = f () in x
