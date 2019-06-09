module BagParse.Parser.Types
  (

    Parser (..)
  , Result (..)
  , Harvest (..)

  ) where

import Data.Bifunctor
import Data.Coerce
import Data.Functor.Compose


--- Types ---

{- | A parser consumes some portion (none, part, or all) or its input (the "bag"), and returns a 'Result' which contains the remaining unconsumed input and whatever was harvested from the bag.

Type parameters:

  - @bag@ - The parser's input (typically some collection like a list, set, or map)
  - @log@ - Any warnings or errors (typically a 'Monoid')
  - @a@ - A value that the parser produces when it successfully parses the bag.

-}

newtype Parser bag log a =
  Parser
    (bag -> Result bag log a)

{- | The result of running a parser.

Type parameters:

  - @bag@ - The parser's input (typically some collection like a list, set, or map)
  - @log@ - Any warnings or errors (typically a 'Monoid')
  - @a@ - A value that is present if the parsing was successful.

-}

data Result bag log a =
  Result
    bag             -- ^ Any unconsumed portion of the input
    (Harvest log a) -- ^ The product of what was reaped from the bag

{- | What a parser has produced from the portion of input that it consumed.

Type parameters:

  - @log@ - Any warnings or errors (typically a 'Monoid')
  - @a@ - A value that is present if the parsing was successful.

-}

data Harvest log a =
  Harvest
    log       -- ^ Any errors or warnings emitted while parsing
    (Maybe a) -- ^ 'Just' for the outcome of a successful parse,
              --   'Nothing' for parse failure.


--- Functors ---

deriving stock instance Functor (Parser bag log)

deriving stock instance Functor (Result bag log)

deriving stock instance Functor (Harvest log)


--- Bifunctors ---

instance Bifunctor Harvest where
    bimap = bimapHarvest

instance Bifunctor (Result bag) where
    bimap = bimapResult

instance Bifunctor (Parser bag) where
    bimap = bimapParser

bimapHarvest :: forall log log' a a'.
    (log -> log') ->
    (a -> a') ->
    Harvest log  a ->
    Harvest log' a'

bimapHarvest f g (Harvest log a) =
    Harvest (f log) (fmap g a)

bimapResult :: forall bag log log' a a'.
    (log -> log') ->
    (a -> a') ->
    Result bag log  a ->
    Result bag log' a'

bimapResult f g (Result bag harvest) =
    Result bag (bimapHarvest f g harvest)

bimapParser :: forall bag log log' a a'.
    (log -> log') ->
    (a -> a') ->
    Parser bag log  a ->
    Parser bag log' a'

bimapParser f g (Parser p) =
    Parser \bag -> bimapResult f g (p bag)


--- Applicative functor ---

instance Monoid log => Applicative (Harvest log) where
    pure = harvestPure
    (<*>) = harvestAp

instance Monoid log => Applicative (Parser bag log) where
    pure = parserPure
    (<*>) = parserAp

harvestPure :: forall log a. Monoid log =>
    a -> Harvest log a

harvestPure x = Harvest mempty (Just x)

parserPure :: forall bag log a. Monoid log =>
    a -> Parser bag log a

parserPure x =
    Parser \bag ->
        Result bag (harvestPure x)

harvestAp :: forall log x a. Semigroup log =>
    Harvest log (x -> a) ->
    Harvest log x ->
    Harvest log a

harvestAp (Harvest log1 f) (Harvest log2 x) =
    Harvest (log1 <> log2) (f <*> x)

parserAp :: forall bag log x a. Semigroup log =>
    Parser bag log (x -> a) ->
    Parser bag log x ->
    Parser bag log a

parserAp (Parser pf) (Parser px) =
    Parser \bag ->
        let
            Result bag'  h1 = pf bag
            Result bag'' h2 = px bag'
        in
            Result bag'' (harvestAp h1 h2)
