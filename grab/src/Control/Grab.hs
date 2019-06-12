{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE

    BangPatterns, BlockArguments, DeriveFunctor,
    DerivingStrategies, GADTs, LambdaCase,
    ScopedTypeVariables, StandaloneDeriving,
    TypeApplications

#-}

module Control.Grab
  (
  -- * Types
  -- ** The Grab type
    Grab (..)
  -- ** Aliases: Simple, Dump, Result, Extract
  , Simple, Dump, Result, Extract

  -- * Creation
  -- ** Making grabs
  , partition, (/)
  -- ** Making dumps
  , dump, discardResidue
  -- ** Making extracts
  , success, failure, warning, extract

  -- * Use
  -- ** Applying a grab to an input
  , runGrab, runDump
  -- ** Deconstructing results
  , residue, log, desideratum

  ) where

import Control.Applicative (Applicative (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (Functor (..))
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Prelude ()


--- The Grab type ---

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
    is a __'Simple'__ grab and it has an 'Applicative' instance.

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
    Applicative (Grab bag residue log)
  where
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

instance Bifunctor (Grab bag residue)
  where
    bimap = bimapGrab

bimapGrab :: forall bag residue log log' a a'.
    (log -> log') -> (a -> a') ->
    Grab bag residue log  a ->
    Grab bag residue log' a'

bimapGrab f g (Grab x) =
    Grab \bag ->
        let
            (bag', lg, a) = x bag
        in
            (bag', f lg, fmap @Maybe g a)


--- Creating grabs ---

-- | The most general way to construct an 'Extract'.
extract
    :: log
        -- ^ Log output, such as an error or warning message.
    -> Maybe desideratum
        -- ^ 'Just' some desideratum if the extract represents the
        --   outcome of a successful grab, or 'Nothing' if it
        --   represents failure.
    -> Extract log desideratum
        -- ^ An extract consisting of the given log and desideratum.

extract x y = Grab \() -> ((), x, y)

success :: Monoid log
    => desideratum
        -- ^ The desired object.
    -> Extract log desideratum
        -- ^ A successful extract with an empty log.

failure
    :: log
        -- ^ Log output such as an error message.
    -> Extract log desideratum
        -- ^ An extract with the given log and no desideratum.

warning
    :: log
        -- ^ Log output such as a warning message.
    -> Extract log ()
        -- ^ An extract with the given log and a desideratum of @()@.

success x = extract mempty (Just x)
failure x = extract x (Nothing)
warning x = extract x (Just ())

partition :: Monoid log
    => (bag -> (desideratum, residue))
        -- ^ Function that partitions the bag into desideratum and residue.
    -> Grab bag residue log desideratum
        -- ^ A grab that always succeeds and never logs.
partition f =
    Grab \i ->
        let
            (s, r) = f i
        in
            (r, mempty, Just s)

dump
    :: (bag -> Extract log desideratum)
        -- ^ A function which, given the entire input, produces
        --   some log output and maybe a desideratum.
    -> Dump bag log desideratum
        -- ^ A grab that consumes the entire bag, producing
        --   whatever the function extracted from its contents.
dump f =
    Grab \i ->
        let
            p = f i
        in
            ((), log p, desideratum p)

-- | @a / b@ is a pipeline of two grabs, using the output of /a/
-- as the input to /b/.
(/) :: Semigroup log
    => Grab bag residue log x
        -- ^ The first grab /a/, whose desideratum @x@ will be
        --   passed as input to the second grab /b/.
    -> Grab x r log desideratum
        -- ^ The second grab /b/. The residue of this grab will be
        --   ignored, so it usually ought to be a 'Dump'.
    -> Grab bag residue log desideratum
        -- ^ A grab whose result is the residue of /a/, the combined
        --   logs of both /a/ and /b/, and the desideratum of /b/.

(/) (Grab f) (Grab g) =
    Grab \i ->
        let
            (x, y, z) = f i
        in
            case z of
                Nothing -> (x, y, Nothing)
                Just a ->
                    let
                        (_, y', z') = g a
                    in
                        (x, y <> y', z')

discardResidue
    :: Grab bag residue log desideratum
        -- ^ A grab which may produce some residue.
    -> Dump bag log desideratum
        -- ^ A grab that produces no residue.
discardResidue (Grab f) =
    Grab \bag ->
        let
            (_, y, z) = f bag
        in
            ((), y, z)


--- Using grabs ---

-- | When @residue@ is @()@, this function specializes to 'runDump'.
runGrab
    :: bag
        -- ^ The input.
    -> Grab bag residue log desideratum
        -- ^ A grab, which may consume some portion of the input.
    -> Result residue log desideratum
        -- ^ The result of performing the grab, which consists of
        --   the @residue@ representing the remaining portion of
        --   input, a @log@ for providing error output, and a
        --   @desideratum@ if the grab was successful.
runGrab x (Grab f) =
    let
        !r = f x
    in
        Grab \() -> r

-- | This is a specialization of the more general 'runGrab' function.
runDump
    :: bag
        -- ^ The input.
    -> Dump bag log desideratum
        -- ^ A dump which consumes the input.
    -> Extract log desideratum
        -- ^ The result extracted from the input, which
        --   consists of a @log@ for providing error output
        --   and a @desideratum@ if the grab was successful.

runDump = runGrab

desideratum
    :: Result residue log desideratum
        -- ^ Either a 'Result' or an 'Extract'.
    -> Maybe desideratum
        -- ^ The desired object, if one was successfully
        --   extracted from the bag.

log :: Result residue log desideratum
        -- ^ Either a 'Result' or an 'Extract'.
    -> log
        -- ^ Any extra information produced during the
        --   grab, such as error messages.

residue
    :: Result residue log desideratum
        -- ^ The result of 'run'ning a 'Grab'
    -> residue
        -- ^ The portion of the bag that was not consumed
        --   by the grab.

residue     (Grab f) = let (x, _, _) = f () in x
log         (Grab f) = let (_, x, _) = f () in x
desideratum (Grab f) = let (_, _, x) = f () in x
