module Control.Grab
  (
  -- * The main type
    Grab (..)

  -- * Type aliases
  , Grab', Dump, Result, Product

  -- * Constants
  , nil

  -- * Entering the Action type
  , log, value, logAndValueMaybe, valueMaybe, select, dump

  -- * Within the Action type
  , discardRemainder, run, (>->), mapLog

  -- * Exiting the Action type
  , toValueMaybe, toLogOrValue, toLog, toLogAndValue, toRemainder

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

  - Consumes some portion (none, part, or all) of its @input@;
  - Returns:
    - The @remainder@ of the unconsumed input;
    - Some monoidal @log@ e.g. a list of error messages;
    - Some @value@ produced from the consumed input.

Specializations of this type:

  - If the @input@ and @remainder@ types are the same, the action is a 'Grab'' (a simple grab).
  - If the @remainder@ is @()@, the action is a 'Dump'; it dumps out the entire input so there is nothing remaining.
  - If the input is @()@, the action is just a single fixed 'Result', which consists of the @remainder@, @log@, and @Maybe value@.
  - If both the input and remainder are @()@, the output is just the 'Product', which consists of the @log@ and @Maybe value@.

-}

data Grab input remainder log value =
  Grab
    (input -> (remainder, log, Maybe value))


--- Type aliases ---

{- |

A 'Grab' consumes some portion (none, part, or all) of its input, and returns a 'Result' which contains the remaining unconsumed input and whatever was yielded from the bag.

Type parameters:

  - @bag@ - The grab's input (typically some collection like a list, set, or map)
  - @log@ - Any warnings or errors (typically a 'Monoid')
  - @a@ - A value that the grab produces when it successfully grabs from the bag.

-}

type Grab' bag log value = Grab bag bag log value

{- | The result of performing a 'Grab'. -}

type Result remainder log value = Grab () remainder log value

{- | A 'Dump' is a 'Grab' the consumes the entire bag. -}

type Dump input log value = Grab input () log value

{- | The outcome produced by performing a 'Dump'. -}

type Product log value = Grab () () log value


--- Functor ---

deriving stock instance Functor (Grab input remainder log)


--- Applicative functor ---

instance (input ~ remainder, Monoid log) =>
  Applicative (Grab input remainder log) where
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

instance Bifunctor (Grab input remainder) where
    bimap = bimapGrab

bimapGrab :: forall input remainder log log' a a'.
    (log -> log') ->
    (a -> a') ->
    Grab input remainder log  a ->
    Grab input remainder log' a'

bimapGrab f g (Grab x) =
    Grab \bag ->
        let
            (bag', log, a) = x bag
        in
            (bag', f log, fmap @Maybe g a)


--- Entering the Action type ---

nil :: Monoid log => Grab' bag log value
nil = Grab \bag -> (bag, mempty, Nothing)

log :: log -> Product log value
log x = Grab \() -> ((), x, Nothing)

value :: Monoid log => value -> Product log value
value x = valueMaybe (Just x)

valueMaybe :: Monoid log => Maybe value -> Product log value
valueMaybe x = Grab \() -> ((), mempty, x)

logAndValueMaybe :: log -> Maybe value -> Product log value
logAndValueMaybe x y = Grab \() -> ((), x, y)

select :: Monoid log => (input -> (remainder, selection)) ->
                        Grab input remainder log selection
select f = Grab \i -> let (r, s) = f i
                      in  (r, mempty, Just s)

dump :: (input -> Product log value) -> Dump input log value
dump f = Grab \i -> let p = f i
                    in  ((), toLog p, toValueMaybe p)


--- Within the Action type ---

(>->) :: Semigroup log
    => Grab input remainder log x
    -> Grab x r log value
        -- ^ The remainder of this 'Grab' will be ignored, so it usually ought to be a 'Dump'.
    -> Grab input remainder log value

(>->) (Grab f) (Grab g) =
    Grab \i ->
        let (x, y, z) = f i
        in  case z of
                Nothing -> (x, y, Nothing)
                Just a -> let (_, y', z') = g a
                          in  (x, y <> y', z')

run :: input -> Grab input remainder log value ->
                Result remainder log value
run x (Grab f) = Grab \() -> f x

discardRemainder :: Grab input remainder log value ->
                    Dump input log value
discardRemainder (Grab f) =
    Grab \bag -> let (_, y, z) = f bag
                 in  ((), y, z)

mapLog :: (log -> log') -> Grab input remainder log value ->
                           Grab input remainder log' value
mapLog f = first f


--- Exiting the Action type ---

toValueMaybe :: Result remainder log value -> Maybe value
toValueMaybe (Grab f) =
    let (_x, _y, z) = f ()
    in  z

toLogOrValue :: Result remainder log value -> Either log value
toLogOrValue (Grab f) =
    let (_x, y, z) = f ()
    in  case z of Nothing -> Left y
                  Just v -> Right v

toLog :: Result remainder log value -> log
toLog (Grab f) =
    let (_x, y, _z) = f ()
    in  y

toLogAndValue :: Result remainder log value -> (log, Maybe value)
toLogAndValue (Grab f) =
    let (_x, y, z) = f ()
    in  (y, z)

toRemainder :: Result remainder log value -> remainder
toRemainder (Grab f) =
    let (x, _y, _z) = f ()
    in  x
