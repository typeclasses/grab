module BagParse.Parser.Prelude

  ( nil

  -- * Entering the Action type
  , log, value, logAndValueMaybe, valueMaybe, select, dump

  -- * Within the Action type
  , discardRemainder, run, (>->), mapLog

  -- * Exiting the Action type
  , toValueMaybe, toLogOrValue, toLog, toLogAndValue, toRemainder

  ) where

import Prelude (Maybe (..), Either (..), Semigroup (..), Monoid (..))

import BagParse.Parser.Types

import Data.Bifunctor
import Data.Coerce


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

(>->) :: (Semigroup log) => Grab input remainder log x ->
                            Dump x log value ->
                            Grab input remainder log value

(>->) (Grab f) (Grab g) =
    Grab \i ->
        let (x, y, z) = f i
        in  case z of
                Nothing -> (x, y, Nothing)
                Just a -> let ((), y', z') = g a
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
