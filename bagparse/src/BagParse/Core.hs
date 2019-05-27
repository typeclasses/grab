module BagParse.Core where

import Data.Bifunctor
import Data.Void
import Data.Witherable

data Parser (bag :: * -> *) item err a
  where

    -- | A trivial parser consumes no items; it produces a parse result, either failure or success, while leaving the input bag untouched.
    Parser_Trivial
        :: Either err a  -- ^ The result that this parser always produces.
        -> Parser bag item err a

    -- | A selection parser always succeeds. It consumes bag items that match the selection function, produces a bag containing the consumed items, and then applies the extraction function to produce the result.
    Parser_Select
        :: (item -> Maybe x)  -- ^ The selection function. If this function returns @Just@, then the parser consumes the item; if it returns @Nothing@, then the parser leaves the item in the input bag.
        -> (bag x -> a)  -- ^ The extraction function.
        -> Parser bag item err a

    -- | '<*>'
    Parser_Ap
        :: (Parser bag item err (x -> a))
        -> Parser bag item err x
        -> Parser bag item err a

deriving stock instance Functor (Parser bag item err)

instance Applicative (Parser bag item err)
  where
    pure = Parser_Trivial . Right
    (<*>) = Parser_Ap

instance Bifunctor (Parser bag item)
  where
    bimap f g = \case
      Parser_Trivial result -> Parser_Trivial (bimap f g result)
      Parser_Select s e -> Parser_Select s (g . e)
      Parser_Ap p1 p2 -> Parser_Ap (bimap f (fmap g) p1) (first f p2)

class Bag (bag :: * -> *)
  where
    bagSelect :: (a -> Maybe b) -> bag a -> BagSelection bag a b

data BagSelection bag a b
  where
    BagSelection
      :: bag a  -- ^ Unselected items
      -> bag b  -- ^ Selected items
      -> BagSelection bag a b

instance Functor bag => Functor (BagSelection bag a)
  where
    fmap f (BagSelection as bs) = BagSelection as (fmap f bs)

instance Functor bag => Bifunctor (BagSelection bag)
  where
    bimap f g (BagSelection as bs) = BagSelection (fmap f as) (fmap g bs)

instance Bag []
  where
    bagSelect f = \case
        [] -> BagSelection [] []
        x:xs ->
            let BagSelection unselected selected = bagSelect f xs in
            case f x of
                Nothing -> BagSelection (x : unselected) selected
                Just y -> BagSelection unselected (y : selected)

data ParseResult bag item err a
  where
    ParseResult
      :: bag item  -- ^ Unconsumed items
      -> Either err a  -- ^ Failure or success
      -> ParseResult bag item err a

parseResultEither :: ParseResult bag item err a -> Either err a
parseResultEither (ParseResult _ x) = x

parse :: (Bag bag, Semigroup err) => Parser bag item err a -> bag item -> ParseResult bag item err a
parse = \case
    Parser_Trivial result -> \inputs -> ParseResult inputs result
    Parser_Select s e -> \inputs ->
        let BagSelection unconsumed consumed = bagSelect s inputs
        in  ParseResult unconsumed (Right (e consumed))
    Parser_Ap p1 p2 -> \inputs ->
        let ParseResult inputs'  f = parse p1 inputs
            ParseResult inputs'' x = parse p2 inputs'
        in  ParseResult inputs'' (apEither f x)

apEither :: Semigroup e => Either e (a -> b) -> Either e a -> Either e b
apEither (Left e1) (Left e2) = Left (e1 <> e2)
apEither (Left e) _ = Left e
apEither _ (Left e) = Left e
apEither (Right f) (Right x) = Right (f x)

