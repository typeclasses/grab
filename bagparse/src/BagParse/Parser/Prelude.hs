module BagParse.Parser.Prelude
  (

  -- * Constructing parsers
    parser, peek, select, dump

  -- * Running parsers
  , parse, parseYield, parseEither, parseMaybe

  -- * Constructing parse results
  , result

  -- * Consuming parse results
  , resultEither, resultMaybe

  -- * Constructing yields
  , yield, logYield

  ) where

import BagParse.Parser.Types

import Data.Coerce

parser :: (bag -> Result bag log a) -> Parser bag log a
parser = Parser

result :: bag -> Yield log a -> Result bag log a
result = Result

yield :: log -> Maybe a -> Yield log a
yield = Yield

logYield :: log -> Yield log a
logYield log = yield log Nothing

yieldMaybe :: Yield log a -> Maybe a
yieldMaybe (Yield _ x) = x

yieldEither :: Yield log a -> Either log a
yieldEither (Yield log Nothing) = Left log
yieldEither (Yield _ (Just x)) = Right x

resultBag :: Result bag log a -> bag
resultBag (Result bag _ ) = bag

resultYield :: Result bag log a -> Yield log a
resultYield (Result _ yield) = yield

resultMaybe :: Result bag log a -> Maybe a
resultMaybe = yieldMaybe . resultYield

resultEither :: Result bag log a -> Either log a
resultEither = yieldEither . resultYield

-- | Peek inside the bag, but don't take anything.
peek :: Monoid log => Parser bag log bag
peek = parser \bag -> result bag (pure bag)

parse :: Parser bag log a -> bag -> Result bag log a
parse = coerce

parseMaybe :: Parser bag log a -> bag -> Maybe a
parseMaybe p = resultMaybe . parse p

parseEither :: Parser bag log a -> bag -> Either log a
parseEither p = resultEither . parse p

parseYield :: Parser bag log a -> bag -> Yield log a
parseYield p = resultYield . parse p

dump
    :: Monoid bag
    => (bag -> Yield log a)
    -> Parser bag log a

dump f =
    parser \bag ->
        Result mempty (f bag)

select
    :: (bag -> (bag, bag'))
          -- ^ Splits the bag into (unselected, selected).
    -> (bag' -> Yield log a)
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
