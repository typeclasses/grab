module BagParse.Form.Prelude
  (

  -- * Value interpretation
    checkbox
  , text
  , optionalText

  -- * Selecting params by name
  , here
  , at
  , natMap
  , natList

  -- * Failing on unexpected params
  , unexpected
  , only

  -- * Displaying errors
  , englishSentenceLogText

  -- * Names
  , readName
  , showName

  ) where

import BagParse.Form.Types
import BagParse.List.Prelude

import qualified BagParse.List.Types

import Data.Bifunctor
import Data.Coerce
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

showName :: Name -> Text
showName (Name []) = ""
showName (Name (x : xs)) = showNamePart x <> showNameRemainder xs
  where
    showNameRemainder [] = ""
    showNameRemainder (y : ys) = "." <> showNamePart' y <> showNameRemainder ys

    showNamePart (NameStr s) = s
    showNamePart (NameNat n) = showNat n

    showNamePart' (NameStr s) = "." <> s
    showNamePart' (NameNat n) = showNat n

    showNat n = "[" <> Text.pack (show @Natural n) <> "]"

readName :: Text -> Maybe Name
readName =
    Text.span (\c -> not (elem c (".[]" :: [Char]))) >>>
    \case
        (Text.null -> True, _) -> Nothing
        (s, x) -> cons (NameStr s) <$> readNameRemainder x
  where
    cons :: NamePart -> Name -> Name
    cons = coerce ((:) @NamePart)

    readNameRemainder :: Text -> Maybe Name
    readNameRemainder =
        \case
            (Text.null -> True) -> Just (Name [])
            (Text.stripPrefix "." -> Just x) -> readName x
            (Text.stripPrefix "[" -> Just (Text.decimal @Natural -> Right (n, (Text.stripPrefix "]" -> Just x)))) -> cons (NameNat n) <$> readNameRemainder x
            _ -> Nothing

(>>>) :: (a -> b) -> (b -> c) -> (a -> c)

(>>>) =
    flip (.)

(.=) :: Ord err => Name -> err -> Log err

k .= err =
    coerce (Map.singleton k (Set.singleton err))

{- | A parser that fails if the form contains any parameters.

This is usually used at the end of an Applicative chain of parsers to assert that every parameter has been consumed by one of the previous parsers.

It may be more convenient to use 'only'. -}

unexpected :: (Ord err, Err_Unexpected err) =>
    Parser err ()

unexpected =
    dump \case [] -> pure (); xs -> logHarvest (foldMap err xs)
  where
    err (Param k _v) = (k .= err_unexpected)

{- | @only p@ = @p <* 'unexpected'@ -}

only :: (Ord err, Err_Unexpected err) =>
    Parser err a -> Parser err a

only =
    (<* unexpected)

errorHarvest :: Ord err =>
    err -> Harvest err a

errorHarvest x =
    logHarvest (Name [] .= x)

text :: forall err.
    ( Ord err
    , Err_Unexpected err
    , Err_Missing err
    , Err_Duplicate err
    ) =>
    Parser err Text

text = (only . here) (dump f)
  where
    f :: Form -> Harvest err Text
    f = map paramValue >>> unique >>> \case
        []         -> errorHarvest err_missing
        x : []     -> pure x
        _ : _ : [] -> errorHarvest err_duplicate

optionalText ::
    ( Ord err
    , Err_Unexpected err
    , Err_Duplicate err
    ) =>
    Parser err (Maybe Text)

optionalText =
    only . here . dump $
        map paramValue >>> unique >>> \case
            []         -> pure Nothing
            x : []     -> pure (Just x)
            _ : _ : [] -> errorHarvest err_duplicate

checkbox ::
    ( Ord err
    , Err_OnlyAllowed err
    , Err_Unexpected err
    ) =>
    Text ->
    Parser err Bool

checkbox yes =
    only . here . dump $
        map paramValue >>> unique >>> List.partition (== yes) >>> \case
            ( []     , []    ) -> pure False
            ( _ : [] , []    ) -> pure True
            ( _      , _ : _ ) -> errorHarvest (err_onlyAllowed yes)

natMap :: forall err a. (Ord err) =>
    Parser err a ->
    Parser err (Map Natural a)

natMap p = select f g
  where
    f :: Param -> Maybe (Natural, Param)
    g :: [(Natural, Param)] -> Harvest err (Map Natural a)

    f param =
        case (paramName param) of
            Name (NameNat n : xs) -> Just (n, Param (Name xs) (paramValue param))
            _ -> Nothing

    g xs =
        let
            groups :: [(Natural, [Param])]
            groups = Map.toList $
                foldr
                    (Map.unionWith (++))
                    Map.empty
                    (map (\(n, param) -> Map.singleton n [param]) xs)
        in
            (_ :: Harvest err (Map Natural a))

natList :: (Ord err) =>
    Parser err a -> Parser err [a]

natList =
    fmap Foldable.toList . natMap

here :: Ord err =>
    Parser err a -> Parser err a

here p =
    select
    (\param -> if (paramName param == Name []) then Just param else Nothing)
    (\params -> parseHarvest p params)

contextualizeLog :: forall err.
    (Name -> Name) -> Log err -> Log err

contextualizeLog f =
    coerce (alterMapKeys @Name @(Set err)) f

alterMapKeys :: forall k a. Ord k =>
    (k -> k) ->
    Map k a ->
    Map k a

alterMapKeys f =
    Map.fromList . map (first f) . Map.toList

at :: Ord err =>
    Text -> (Form -> Harvest err a) -> Parser err a

at k f =
    first (contextualizeLog (\(Name xs) -> Name (NameStr k : xs))) $
      select
        (\param ->
            case (paramName param) of
                Name (NameStr k' : nameRemainder) | k == k' ->
                    Just (Param (Name nameRemainder) (paramValue param))
                _ -> Nothing
        )
        f

unique :: Ord a =>
    [a] -> [a]

unique =
    Set.toList . Set.fromList

englishSentenceLogText :: Log EnglishSentence -> Text

englishSentenceLogText =
    Text.unlines . map (uncurry f) . Map.toList . coerce
  where
    f :: Name -> Set EnglishSentence -> Text
    f name errs = Text.concat
        [ showName name
        , ": "
        , Text.unwords
            (
              map
                (coerce @EnglishSentence @Text)
                (Set.toList errs)
            )
        ]
