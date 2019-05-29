module BagParse.Parser.State (State (..)) where

import Data.Bifunctor
import Data.Coerce

newtype State s a = State (s -> (s, a)) deriving stock Functor

instance Applicative (State s)
  where
    pure = statePure

    (<*>) :: forall a b. State s (a -> b) -> State s a -> State s b
    (<*>) = coerce (stateAp @s @a @b)

statePure :: forall s a. a -> State s a
statePure x = coerce ((\s -> (s, x)) :: s -> (s, a))

stateAp :: forall s a b. (s -> (s, a -> b)) -> (s -> (s, a)) -> (s -> (s, b))
stateAp sf sx s =
    let (s',  f) = sf s
        (s'', x) = sx s'
    in  (s'', f x)
