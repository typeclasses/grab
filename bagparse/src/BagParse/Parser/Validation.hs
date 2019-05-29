module BagParse.Parser.Validation (Validation (..)) where

import Data.Bifunctor
import Data.Coerce

newtype Validation e a = Validation (Either e a)
    deriving stock Functor
    deriving Bifunctor via Either

instance Semigroup e => Applicative (Validation e) where

    pure :: forall a. a -> Validation e a
    pure = coerce (Right @e @a)

    (<*>) :: forall a b. Validation e (a -> b) -> Validation e a -> Validation e b
    (<*>) = coerce (errorAccumulatingAp @e @a @b)

errorAccumulatingAp :: forall e a b. Semigroup e => Either e (a -> b) -> Either e a -> Either e b
errorAccumulatingAp =
    \case Left e1 -> \case Left e2 -> Left (e1 <> e2)
                           Right _ -> Left e1
          Right f -> \case Left e2 -> Left e2
                           Right x -> Right (f x)
