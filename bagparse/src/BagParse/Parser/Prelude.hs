module BagParse.Parser.Prelude

  ( nil

  -- * Entering the Action type
  , log, value, valueMaybe, select, dump

  -- * Within the Action type
  , discardRemainder, run, (>->)

  -- * Exiting the Action type
  , toValueMaybe, toLogOrValue, toLog, toLogAndValue, toRemainder

  ) where

import Prelude (Maybe (..), Either (..), Semigroup (..), Monoid (..))

import BagParse.Parser.Types

import Data.Coerce


--- Entering the Action type ---

nil :: Monoid log => Action bag bag log value
nil =
    Action \bag ->
        (bag, mempty, Nothing)

log ::
    log ->
    Action () () log value

log x =
    Action \() ->
        ((), x, Nothing)

value :: Monoid log =>
    value ->
    Action () () log value

value x =
    valueMaybe (Just x)

valueMaybe :: Monoid log =>
    Maybe value ->
    Action () () log value

valueMaybe x =
    Action \() ->
        ((), mempty, x)

select :: Monoid log =>
    (input -> (remainder, selection)) ->
    Action input remainder log selection

select f =
    Action \i ->
        let
            (r, s) = f i
        in
            (r, mempty, Just s)

dump ::
    (input -> Product log value) ->
    Dump input log value

dump f =
    Action \i ->
        let
            p = f i
        in
            ((), toLog p, toValueMaybe p)


--- Within the Action type ---

(>->) :: (Semigroup log) =>
    Action input remainder log x ->
    Dump x log value ->
    Action input remainder log value

(>->) (Action f) (Action g) =
    Action \i ->
        let
            (x, y, z) = f i
        in
            case z of
                Nothing -> (x, y, Nothing)
                Just a ->
                    let
                        ((), y', z') = g a
                    in
                        (x, y <> y', z')

run ::
    input ->
    Action input remainder log value ->
    Action ()    remainder log value

run x (Action f) =
    Action \() ->
        f x

discardRemainder ::
    Action input remainder log value ->
    Action input ()        log value

discardRemainder (Action f) =
    Action \bag ->
        let
            (_, y, z) = f bag
        in
            ((), y, z)


--- Exiting the Action type ---

toValueMaybe ::
    Action () remainder log value ->
    Maybe value

toValueMaybe (Action f) =
    let
        (_x, _y, z) = f ()
    in
        z

toLogOrValue ::
    Action () remainder log value ->
    Either log value

toLogOrValue (Action f) =
    let
        (_x, y, z) = f ()
    in
        case z of
            Nothing -> Left y
            Just v -> Right v

toLog ::
    Action () remainder log value ->
    log

toLog (Action f) =
    let
        (_x, y, _z) = f ()
    in
        y

toLogAndValue ::
    Action () remainder log value ->
    (log, Maybe value)

toLogAndValue (Action f) =
    let
        (_x, y, z) = f ()
    in
        (y, z)

toRemainder ::
    Action () remainder log value ->
    remainder

toRemainder (Action f) =
    let
        (x, _y, _z) = f ()
    in
        x

{-

grab :: (bag -> Result bag log a) -> Grab bag log a
grab f = _

grabResult :: bag -> Yield log a -> GrabResult bag log a
grabResult = GrabResult

yield :: log -> Maybe a -> Yield log a
yield = Yield

logYield :: log -> Yield log a
logYield log = yield log Nothing

yieldMaybe :: Yield log a -> Maybe a
yieldMaybe (Yield _ x) = x

yieldEither :: Yield log a -> Either log a
yieldEither (Yield log Nothing) = Left log
yieldEither (Yield _ (Just x)) = Right x

grabResultBag :: GrabResult bag log a -> bag
grabResultBag (GrabResult bag _ ) = bag

grabResultYield :: GrabResult bag log a -> Yield log a
grabResultYield (GrabResult _ yield) = yield

grabResultMaybe :: GrabResult bag log a -> Maybe a
grabResultMaybe = yieldMaybe . grabResultYield

grabResultEither :: GrabResult bag log a -> Either log a
grabResultEither = yieldEither . grabResultYield

-- | Peek inside the bag, but don't take anything.
peek :: Monoid log => Grab bag log bag
peek = Grab \bag -> GrabResult bag (pure bag)

runGrab :: Grab bag log a -> bag -> GrabResult bag log a
runGrab = coerce

runGrabMaybe :: Grab bag log a -> bag -> Maybe a
runGrabMaybe p = grabResultMaybe . runGrab p

runGrabEither :: Grab bag log a -> bag -> Either log a
runGrabEither p = grabResultEither . runGrab p

runGrabYield :: Grab bag log a -> bag -> Yield log a
runGrabYield p = grabResultYield . runGrab p

runDump :: Dump bag log a -> bag -> Yield log a
runDump = coerce

dump :: (bag -> Yield log a) -> Dump bag log a
dump = coerce

grabAll
    :: Monoid bag
    => Dump bag log a
    -> Grab bag log a

grabAll (Dump f) =
    Grab \bag ->
        grabResult mempty (f bag)

grabSome
    :: (bag -> (bag, bag'))
          -- ^ Splits the bag into (unselected, selected).
    -> Dump bag' log a
    -> Grab bag log a

grabSome f d = grab \bag ->
    let
        (unselected, selected) = f bag
    in
        grabResult unselected (runDump d selected)

-}
