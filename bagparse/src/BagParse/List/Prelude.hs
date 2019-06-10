module BagParse.List.Prelude

  ( X.nil

  -- * Entering the Action type
  , X.log, X.value, select

  -- * Within the Action type
  , X.discardRemainder, X.run, (X.>->)

  -- * Exiting the Action type
  , X.toValueMaybe, X.toLog, X.toLogOrValue, X.toLogAndValue, X.toRemainder

  ) where

import qualified BagParse.Parser.Prelude as X

import BagParse.List.Types

import Data.Function (fix)

select :: Monoid log =>
    (item -> Maybe item') ->
    Action [item] [item] log [item']

select f =
    Action \xs ->
        let
            (r, s) = partitionMaybe f xs
        in
            (r, mempty, Just s)

partitionMaybe :: (a -> Maybe b) -> [a] -> ([a], [b])
partitionMaybe f = fix \r ->
  \case
    [] -> ([], [])
    x : xs ->
        let
          (as, bs) = r xs
        in
          case f x of
            Nothing -> (x : as, bs)
            Just y  -> (as, y : bs)
