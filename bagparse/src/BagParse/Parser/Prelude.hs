module BagParse.Parser.Prelude (parser, peek, parse, parseEither, parseMaybe, select) where

import BagParse.Parser.Types

import Data.Coerce

parser :: (bag -> (bag, Either err a)) -> Parser bag err a
parser = Parser

-- | Peek inside the bag, but don't take anything.
peek :: Parser bag err bag
peek = parser (\bag -> (bag, Right bag))

parse :: Semigroup err => Parser bag err a -> bag -> (bag, Either err a)
parse = coerce

parseEither :: Semigroup err => Parser bag err a -> bag -> Either err a
parseEither p = snd . parse p

parseMaybe :: Semigroup err => Parser bag err a -> bag -> Maybe a
parseMaybe p = viewRight . parseEither p

viewRight :: Either a b -> Maybe b
viewRight = \case Left _ -> Nothing; Right x -> Just x

select :: Semigroup err => (bag -> (bag, bag')) -> Parser bag' err a -> Parser bag err a
select f p = parser \bag ->
  let (unselected, selected) = f bag
      (_, result) = parse p selected
  in  (unselected, result)

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
