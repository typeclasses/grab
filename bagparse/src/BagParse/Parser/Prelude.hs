module BagParse.Parser.Prelude
  (

  -- * Constructing parsers
    parser, peek, select, dump

  -- * Running parsers
  , parse, parseHarvest, parseEither, parseMaybe

  -- * Constructing parse results
  , result

  -- * Consuming parse results
  , resultEither, resultMaybe

  -- * Constructing harvests
  , harvest, logHarvest

  ) where

import BagParse.Parser.Types

import Data.Coerce

parser :: (bag -> Result bag log a) -> Parser bag log a
parser = Parser

result :: bag -> Harvest log a -> Result bag log a
result = Result

harvest :: log -> Maybe a -> Harvest log a
harvest = Harvest

logHarvest :: log -> Harvest log a
logHarvest log = harvest log Nothing

harvestMaybe :: Harvest log a -> Maybe a
harvestMaybe (Harvest _ x) = x

harvestEither :: Harvest log a -> Either log a
harvestEither (Harvest log Nothing) = Left log
harvestEither (Harvest _ (Just x)) = Right x

resultBag :: Result bag log a -> bag
resultBag (Result bag _ ) = bag

resultHarvest :: Result bag log a -> Harvest log a
resultHarvest (Result _ harvest) = harvest

resultMaybe :: Result bag log a -> Maybe a
resultMaybe = harvestMaybe . resultHarvest

resultEither :: Result bag log a -> Either log a
resultEither = harvestEither . resultHarvest

-- | Peek inside the bag, but don't take anything.
peek :: Monoid log => Parser bag log bag
peek = parser \bag -> result bag (pure bag)

parse :: Parser bag log a -> bag -> Result bag log a
parse = coerce

parseMaybe :: Parser bag log a -> bag -> Maybe a
parseMaybe p = resultMaybe . parse p

parseEither :: Parser bag log a -> bag -> Either log a
parseEither p = resultEither . parse p

parseHarvest :: Parser bag log a -> bag -> Harvest log a
parseHarvest p = resultHarvest . parse p

dump
    :: Monoid bag
    => (bag -> Harvest log a)
    -> Parser bag log a

dump f =
    parser \bag ->
        Result mempty (f bag)

select
    :: (bag -> (bag, bag'))
          -- ^ Splits the bag into (unselected, selected).
    -> (bag' -> Harvest log a)
    -> Parser bag  log a

select f g = parser \bag ->
    let
        (unselected, selected) = f bag
        h = g selected
    in
        result unselected h

{-

trivialP_maybe :: Maybe a -> Parser bag () a
trivialP_maybe = \case Just x -> parseSuccess x; Nothing -> parseFailure ()

trivialP_either :: Either err a -> Parser bag err a
trivialP_either = \case Left x -> parseFailure x; Right x -> parseSuccess x

nothing :: err -> a -> Parser [x] err a
nothing err x = Parser
  \case [] -> ([], Right x)
        _  -> ([], Left err)

parseSuccess :: a -> Parser bag err a
parseSuccess x = Parser (\bag -> (bag, Right x))

parseFailure :: err -> Parser bag err a
parseFailure err = Parser (\bag -> (bag, Left err))

one :: err -> Parser [a] err a
one err = Parser \case
  [x] -> ([], Right x)
  _ -> ([], Left err)

parseResultEither :: ParseResult bag err a -> Either err a
parseResultEither (ParseResult _ x) = x

data ParseResult bag err a = ParseResult bag (Either err a)

parse :: Semigroup err => Parser bag err a -> bag -> ParseResult bag err a
parse p bag =
    let (bag', value) = coerce p bag
    in  ParseResult bag' value

-}
