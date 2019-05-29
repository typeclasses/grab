module BagParse.Parser.Types (Parser (..)) where

import BagParse.Parser.State
import BagParse.Parser.Validation

import Data.Bifunctor
import Data.Coerce
import Data.Functor.Compose

newtype Parser bag err a = Parser (bag -> (bag, Either err a))
    deriving stock Functor
    deriving Applicative via Compose (State bag) (Validation err)

instance Bifunctor (Parser bag) where bimap = bimapParser

bimapParser :: forall bag a a' b b'. (a -> a') -> (b -> b') -> Parser bag a b -> Parser bag a' b'
bimapParser = coerce (bimapCompose @(State bag) @Validation @a @a' @b @b')

bimapCompose :: forall f g a a' b b'. Functor f => Bifunctor g =>
    (a -> a') -> (b -> b') -> f (g a b) -> f (g a' b')
bimapCompose l r = fmap (bimap l r)
