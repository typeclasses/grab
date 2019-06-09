module BagParse.List.Prelude
  (

  -- * Constructing parsers
    parser, select, dump

  -- * Running parsers
  , parse, parseHarvest

  -- * Constructing results
  , result

  -- * Constructing harvests
  , BagParse.Parser.Prelude.harvest
  , BagParse.Parser.Prelude.logHarvest

  ) where

import qualified BagParse.Parser.Prelude

import BagParse.List.Types

import Data.Function (fix)

parser :: ([item] -> Result item log a) -> Parser item log a
parser = BagParse.Parser.Prelude.parser

result :: [item] -> Harvest log a -> Result item log a
result = BagParse.Parser.Prelude.result

select :: forall item item' log a.
    (item -> Maybe item') ->
    ([item'] -> Harvest log a) ->
    Parser item log a

select f = BagParse.Parser.Prelude.select (partitionMaybe f)

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

-- | Consume all of the items. (Turn the bag over and dump out all of its contents.)
dump
    :: ([item] -> Harvest log a)
    -> Parser item log a

dump = BagParse.Parser.Prelude.dump

parse :: Parser item log a -> [item] -> Result item log a
parse = BagParse.Parser.Prelude.parse

parseHarvest :: Parser item log a -> [item] -> Harvest log a
parseHarvest = BagParse.Parser.Prelude.parseHarvest
