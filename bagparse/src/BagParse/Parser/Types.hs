module BagParse.Parser.Types
  (
  -- * The main type
    Action (..)

  -- * Type aliases
  , Grab, Dump, Result, Product

  ) where

import Data.Bifunctor
import Data.Coerce
import Data.Functor.Compose


--- The main type ---

{- |

An 'Action':

  - Consumes some portion (none, part, or all) of its @input@;
  - Returns:
    - The @remainder@ of the unconsumed input;
    - Some monoidal @log@ e.g. a list of error messages;
    - Some @value@ produced from the consumed input.

Specializations of this type:

  - If the @input@ and @remainder@ types are the same, the action is a 'Grab'.
  - If the @remainder@ is @()@, the action is a 'Dump'; it dumps out the entire input so there is nothing remaining.
  - If the input is @()@, the action is just a single fixed 'Result', which consists of the @remainder@, @log@, and @Maybe value@.
  - If both the input and remainder are @()@, the output is just the 'Product', which consists of the @log@ and @Maybe value@.

-}

data Action input remainder log value =
  Action
    (input -> (remainder, log, Maybe value))


--- Type aliases ---

{- |

A 'Grab' consumes some portion (none, part, or all) of its input, and returns a 'Result' which contains the remaining unconsumed input and whatever was yielded from the bag.

Type parameters:

  - @bag@ - The grab's input (typically some collection like a list, set, or map)
  - @log@ - Any warnings or errors (typically a 'Monoid')
  - @a@ - A value that the grab produces when it successfully grabs from the bag.

-}

type Grab bag log value = Action bag bag log value

{- | The result of performing a 'Grab'. -}

type Result remainder log value = Action () remainder log value

{- | A 'Dump' is a 'Grab' the consumes the entire bag. -}

type Dump input log value = Action input () log value

{- | The outcome produced by performing a 'Dump'. -}

type Product log value = Action () () log value


--- Functor ---

deriving stock instance Functor (Action input remainder log)


--- Applicative functor ---

instance (input ~ remainder, Monoid log) =>
  Applicative (Action input remainder log) where
    pure = actionPure
    (<*>) = actionAp

actionPure :: Monoid log => a -> Action bag bag log a
actionPure a = Action \bag -> (bag, mempty, Just a)

actionAp :: Monoid log =>
    Action bag bag log (x -> a) ->
    Action bag bag log x ->
    Action bag bag log a

actionAp (Action pf) (Action px) =
    Action \bag ->
        let
            (bag',  log1, f) = pf bag
            (bag'', log2, x) = px bag'
        in
            (bag'', log1 <> log2, f <*> x)


--- Bifunctor ---

instance Bifunctor (Action input remainder) where
    bimap = bimapAction

bimapAction :: forall input remainder log log' a a'.
    (log -> log') ->
    (a -> a') ->
    Action input remainder log  a ->
    Action input remainder log' a'

bimapAction f g (Action x) =
    Action \bag ->
        let
            (bag', log, a) = x bag
        in
            (bag', f log, fmap @Maybe g a)
