module BagParse.Parser.Types
  (

    Parser (..)
  , Result (..)
  , Yield (..)

  ) where

import Data.Bifunctor
import Data.Coerce
import Data.Functor.Compose


--- Types ---

{- | A parser consumes some portion (none, part, or all) or its input (the "bag"), and returns a 'Result' which contains the remaining unconsumed input and whatever was yielded from the bag.

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
    (Yield log a) -- ^ The product of what was reaped from the bag

{- | What a parser has produced from the portion of input that it consumed.

Type parameters:

  - @log@ - Any warnings or errors (typically a 'Monoid')
  - @a@ - A value that is present if the parsing was successful.

-}

data Yield log a =
  Yield
    log       -- ^ Any errors or warnings emitted while parsing
    (Maybe a) -- ^ 'Just' for the outcome of a successful parse,
              --   'Nothing' for parse failure.


--- Functors ---

deriving stock instance Functor (Parser bag log)

deriving stock instance Functor (Result bag log)

deriving stock instance Functor (Yield log)


--- Bifunctors ---

instance Bifunctor Yield where
    bimap = bimapYield

instance Bifunctor (Result bag) where
    bimap = bimapResult

instance Bifunctor (Parser bag) where
    bimap = bimapParser

bimapYield :: forall log log' a a'.
    (log -> log') ->
    (a -> a') ->
    Yield log  a ->
    Yield log' a'

bimapYield f g (Yield log a) =
    Yield (f log) (fmap g a)

bimapResult :: forall bag log log' a a'.
    (log -> log') ->
    (a -> a') ->
    Result bag log  a ->
    Result bag log' a'

bimapResult f g (Result bag yield) =
    Result bag (bimapYield f g yield)

bimapParser :: forall bag log log' a a'.
    (log -> log') ->
    (a -> a') ->
    Parser bag log  a ->
    Parser bag log' a'

bimapParser f g (Parser p) =
    Parser \bag -> bimapResult f g (p bag)


--- Applicative functor ---

instance Monoid log => Applicative (Yield log) where
    pure = yieldPure
    (<*>) = yieldAp

instance Monoid log => Applicative (Parser bag log) where
    pure = parserPure
    (<*>) = parserAp

yieldPure :: forall log a. Monoid log =>
    a -> Yield log a

yieldPure x = Yield mempty (Just x)

parserPure :: forall bag log a. Monoid log =>
    a -> Parser bag log a

parserPure x =
    Parser \bag ->
        Result bag (yieldPure x)

yieldAp :: forall log x a. Semigroup log =>
    Yield log (x -> a) ->
    Yield log x ->
    Yield log a

yieldAp (Yield log1 f) (Yield log2 x) =
    Yield (log1 <> log2) (f <*> x)

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
            Result bag'' (yieldAp h1 h2)
