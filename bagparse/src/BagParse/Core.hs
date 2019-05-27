module BagParse.Core where

import Data.Bifunctor
import Data.Maybe

data Parser (bag :: * -> *) item err a where
  Parser_Failure :: err -> Parser bag item err a
  Parser_Success :: a -> Parser bag item err a
  Parser_Select :: (item -> Maybe a) -> Parser bag item err (bag a)
  Parser_Ap :: (Parser bag item err (x -> a))
            -> Parser bag item err x
            -> Parser bag item err a

instance Functor (Parser bag item err) where
instance Applicative (Parser bag item err) where
instance Bifunctor (Parser bag item) where

class Bag (bag :: * -> *) where
  bag_collect :: (a -> Maybe b) -> bag a -> bag b

instance Bag [] where
  bag_collect = Data.Maybe.mapMaybe

data ParseResult item err a =
  Parse_Error

parse :: Bag bag => Parser bag item err a -> bag item -> Either err a
parse = \case
  Parser_Failure err -> const (Left err)
  Parser_Success x -> const (Right x)
  Parser_Select f -> Right . bag_collect f
