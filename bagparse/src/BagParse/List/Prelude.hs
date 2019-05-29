module BagParse.List.Prelude (parser, select, dump) where

import qualified BagParse.Parser.Prelude
import BagParse.List.Types

import Data.Function (fix)

parser :: ([item] -> ([item], Either err a)) -> Parser item err a
parser = BagParse.Parser.Prelude.parser

select :: Semigroup err => (item -> Maybe item') -> Parser item' err a -> Parser item err a
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
dump :: ([item] -> Either err a) -> Parser item err a
dump f = parser \bag -> ([], f bag)
