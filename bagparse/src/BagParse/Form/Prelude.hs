module BagParse.Form.Prelude
  ( module BagParse.Form.Name
  , module BagParse.Form.Errors
  , module BagParse.Form.English

  , X.nil

  -- * Simple form fields
  , text, optionalText, checkbox

  -- * Entering the Action type
  , X.log, X.value, X.select, at, here

  -- * Within the Action type
  , X.discardRemainder, X.run, (>->), only, natList, natListWithIndex

  -- * Exiting the Action type
  , X.toValueMaybe, X.toLogOrValue, X.toLogAndValue, X.toLog, X.toRemainder


  ) where

import BagParse.Form.English
import BagParse.Form.Errors
import BagParse.Form.Input
import BagParse.Form.Log
import BagParse.Form.Types
import BagParse.Form.Name

import qualified BagParse.Parser.Prelude as X
import BagParse.Parser.Prelude ((>->))

import Data.Bifunctor
import Data.Coerce
import Data.Function
import Numeric.Natural

import qualified Data.Foldable as Foldable

import qualified Data.List as List

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

at :: Ord err =>
    NamePart ->
    Grab err Form

at k =
    X.select (formPrefixPartition k)

formPrefixPartition :: NamePart -> Form -> (Form, Form)
formPrefixPartition k (Form xs ctx) =
    let
        (r, s) = partitionMaybe (namePrefixPartition k) xs
    in
        (Form r ctx, Form s (ctx . coerce (k :)))

namePrefixPartition :: NamePart -> Param -> Maybe Param
namePrefixPartition k (Param name value) =
    case name of
        Name (x : xs) | x == k ->
            Just (Param (Name xs) value)
        _ ->
            Nothing

here :: Ord err =>
    Grab err Form

here =
    X.select \(Form xs ctx) ->
        let
            (r, s) = partitionMaybe herePartition xs
        in
            (Form r ctx, Form s ctx)

herePartition :: Param -> Maybe Param
herePartition p@(Param name value) =
    case name of
        Name [] -> Just p
        _ -> Nothing

partitionMaybe :: (a -> Maybe b) -> [a] -> ([a], [b])
partitionMaybe f = fix \r ->
    \case
        [] -> ([], [])
        x : xs ->
            let
              (as, bs) = r xs
            in
              case f x of
                Nothing -> (x : as, bs)
                Just y  -> (as, y : bs)

natListWithIndex :: forall err a. Ord err =>
    Dump err a ->
    Grab err [(Natural, a)]

natListWithIndex d =
    X.select selectNats
    >->
    X.dump \(xs, ctx) ->
        let
            groups :: [(Natural, [Param])]
            groups = Map.toList $
                foldr
                    (Map.unionWith (++))
                    Map.empty
                    (map (\(n, param) -> Map.singleton n [param]) xs)

            forms :: [(Natural, Form)]
            forms =
                map
                    (\(n, xs') -> (n, Form xs' (ctx . coerce (NameNat n :))))
                    groups

            results :: [(Natural, Product err a)]
            results =
                map
                    (\(n, f) -> (n, X.run f (X.mapLog (contextualizeLog (NameNat n)) d)))
                    forms
        in
            X.logAndValueMaybe
                (foldMap (\(_, r) -> X.toLog r) results)
                (allJusts (
                    map
                        (\(n, r) ->
                            case X.toValueMaybe r of
                                Nothing -> Nothing
                                Just v -> Just (n, v)
                        )
                        results
                ))

allJusts :: [Maybe a] -> Maybe [a]
allJusts [] = Just []
allJusts (Nothing : _) = Nothing
allJusts (Just x : xs) = (x :) <$> allJusts xs

selectNats :: Form -> (Form, ([(Natural, Param)], Name -> Name))
selectNats (Form xs ctx) =
    let
        (r, s) = partitionMaybe f xs
    in
        (Form r ctx, (s, ctx))
  where
    f :: Param -> Maybe (Natural, Param)
    f (Param (Name (NameNat n : ns)) v) = Just (n, Param (Name ns) v)
    f _ = Nothing

natList :: Ord err =>
    Dump err a ->
    Grab err [a]

natList d =
    (map snd) <$> natListWithIndex d

text :: forall err.
    (Ord err, Err_Missing err, Err_Duplicate err) =>
    Grab err Text

text = here >-> X.dump f
  where
    f :: Form -> Product err Text
    f (Form xs ctx) =
        case unique (map paramValue xs) of
            []         -> X.log (ctx (Name []) .= err_missing)
            x : []     -> X.value x
            _ : _ : [] -> X.log (ctx (Name []) .= err_duplicate)

optionalText :: forall err.
    (Ord err, Err_Duplicate err) =>
    Grab err (Maybe Text)

optionalText = here >-> X.dump f
  where
    f :: Form -> Product err (Maybe Text)
    f (Form xs ctx) =
        case unique (map paramValue xs) of
            []         -> X.value Nothing
            x : []     -> X.value (Just x)
            _ : _ : [] -> X.log (ctx (Name []) .= err_duplicate)

checkbox :: forall err.
    (Ord err, Err_OnlyAllowed err) =>
    Text ->
    Grab err Bool

checkbox yes = here >-> X.dump f
  where
    f :: Form -> Product err Bool
    f (Form xs ctx) =
        case List.partition (== yes) (unique (map paramValue xs)) of
            ( []     , []    ) -> X.value False
            ( _ : [] , []    ) -> X.value True
            ( _      , _ : _ ) -> X.log (ctx (Name []) .= err_onlyAllowed yes)

only :: forall err a.
    (Ord err, Err_Unexpected err) =>
    Grab err a ->
    Dump err a

only g =
    X.dump \i ->
        let
            r = X.run i g
        in
            case X.toRemainder r of
                Form [] _ -> X.discardRemainder r
                Form xs ctx ->
                    X.logAndValueMaybe
                        (X.toLog r <> foldMap (\(Param n _) -> ctx n .= err_unexpected) xs)
                        (X.toValueMaybe r)

(.=) :: Ord err => Name -> err -> Log err

k .= err =
    coerce (Map.singleton k (Set.singleton err))

unique :: Ord a =>
    [a] -> [a]

unique =
    Set.toList . Set.fromList
