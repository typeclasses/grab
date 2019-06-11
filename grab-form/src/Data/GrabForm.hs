{-# LANGUAGE

    BlockArguments, DerivingStrategies, GeneralizedNewtypeDeriving,
    LambdaCase, OverloadedStrings, ScopedTypeVariables,
    StandaloneDeriving, TypeApplications, ViewPatterns

#-}

module Data.GrabForm
  (
  -- * Form
    Form (..), Param (..)

  -- * Name
  , Name (..), NamePart (..)
  , showName, readName

  -- * Log
  , Log (..), contextualizeLog

  -- * Errors
  , Err_Missing (..)
  , Err_Duplicate (..)
  , Err_Unexpected (..)
  , Err_OnlyAllowed (..)

  -- * English
  , EnglishSentence (..)
  , englishSentenceLogText

  -- * Grab types
  , Grab, Dump

  -- * Parameter name selection
  , at, here, (/)

  -- * Simple form fields
  , text, optionalText, checkbox

  -- * Lists
  , natList, natListWithIndex

  -- * Dealing with unrecognized parameters
  , only, etAlia

  -- * Running grabs
  , grabParams, dumpParams

  ) where

import Prelude hiding ((/))

import Data.Coerce (coerce)
import Data.Bifunctor (Bifunctor (..))
import Data.Function (fix)
import Data.String (IsString (fromString))

import Numeric.Natural (Natural)

import qualified Data.List as List

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Control.Grab as Grab


--- Form ---

data Form =
  Form
    { formParams :: [Param]
    , formContext :: Name -> Name
    }

data Param =
  Param
    { paramName  :: Name
    , paramValue :: Text
    }

deriving stock instance Eq Param
deriving stock instance Ord Param
deriving stock instance Show Param


--- Name ---

newtype Name = Name [NamePart]

data NamePart
  = NameStr Text
  | NameNat Natural
  | NameErr Text

instance IsString NamePart where
  fromString = NameStr . Text.pack

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

readName :: Text -> Name
readName =
    \case
        (Text.null -> True, r) -> Name [NameErr r]
        (s, x) -> cons (NameStr s) (readNameRemainder x)
    .
    Text.span (\c -> not (elem c (".[]" :: [Char])))

  where
    cons :: NamePart -> Name -> Name
    cons = coerce ((:) @NamePart)

    readNameRemainder :: Text -> Name
    readNameRemainder =
        \case
            (Text.null -> True) -> Name []
            (Text.stripPrefix "." -> Just x) -> readName x
            (stripNat -> Just (n, x)) -> cons (NameNat n) (readNameRemainder x)
            t -> Name [NameErr t]

stripNat :: Text -> Maybe (Natural, Text)
stripNat (
    Text.stripPrefix "[" -> Just (
    Text.decimal @Natural -> Right (n, (
    Text.stripPrefix "]" -> Just x)))) =
        Just (n, x)
stripNat _ = Nothing

deriving stock instance Eq NamePart
deriving stock instance Ord NamePart
deriving stock instance Show NamePart

deriving stock instance Eq Name
deriving stock instance Ord Name
deriving stock instance Show Name


--- Log ---

newtype Log a =
  Log
    (Map Name (Set a))

contextualizeLog ::
    NamePart -> Log a -> Log a

contextualizeLog x =
    Log .
    Map.fromList .
    map (first (\(Name xs) -> Name (x : xs))) .
    Map.toList .
    (\(Log m) -> m)

instance Ord a => Semigroup (Log a) where
    (<>) = coerce multimapUnion

instance Ord a => Monoid (Log a) where
    mempty = coerce multimapEmpty

type Multimap a b = Map a (Set b)

multimapEmpty :: Multimap a b
multimapEmpty = Map.empty

multimapUnion :: (Ord a, Ord b) =>
    Multimap a b ->
    Multimap a b ->
    Multimap a b

multimapUnion = Map.unionWith (<>)

deriving stock instance Eq a => Eq (Log a)


--- Errors ---

class Err_Missing a where
    -- | A parameter was expected, but none was given.
    err_missing :: a

class Err_Duplicate a where
    -- | A parameter was given repeatedly in a situation where it was expected to be present at most once.
    err_duplicate :: a

class Err_Unexpected a where
    -- | An unexpected parameter was given.
    err_unexpected :: a

class Err_OnlyAllowed a where
    -- | There is only one allowed value for a parameter, and something different was given.
    err_onlyAllowed :: Text {- ^ The allowed value -} -> a

instance Err_Missing () where err_missing = ()
instance Err_Duplicate () where err_duplicate = ()
instance Err_Unexpected () where err_unexpected = ()
instance Err_OnlyAllowed () where err_onlyAllowed = const ()


--- English ---

newtype EnglishSentence = EnglishSentence Text

deriving newtype instance IsString EnglishSentence

deriving stock instance Eq EnglishSentence
deriving stock instance Ord EnglishSentence

instance Err_Missing EnglishSentence where err_missing = "Required parameter is missing."
instance Err_Duplicate EnglishSentence where err_duplicate = "Parameter may not appear more than once."
instance Err_Unexpected EnglishSentence where err_unexpected = "Unexpected parameter."
instance Err_OnlyAllowed EnglishSentence where err_onlyAllowed value = EnglishSentence ("The only allowed value is `" <> value <> "`.")

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


--- Grab types ---

type Grab err a =
    Grab.Simple Form (Log err) a

type Dump err a =
    Grab.Dump Form (Log err) a

type Result err a =
    Grab.Result Form (Log err) a

type Extract err a =
    Grab.Extract (Log err) a


--- Parameter name selection ---

at :: Ord err => NamePart -> Grab err Form
at k = Grab.partition (formPrefixPartition k)

  where
    formPrefixPartition :: NamePart -> Form -> (Form, Form)
    formPrefixPartition k (Form xs ctx) =
        let
            (s, r) = partitionMaybe (namePrefixPartition k) xs
        in
            (Form s (ctx . coerce (k :)), Form r ctx)

    namePrefixPartition :: NamePart -> Param -> Maybe Param
    namePrefixPartition k (Param name value) =
        case name of
            Name (x : xs) | x == k ->
                Just (Param (Name xs) value)
            _ ->
                Nothing

here :: Ord err => Grab err Form
here =
    Grab.partition \(Form xs ctx) ->
        let
            (s, r) = partitionMaybe herePartition xs
        in
            (Form s ctx, Form r ctx)

  where
    herePartition :: Param -> Maybe Param
    herePartition p@(Param name value) =
        case name of
            Name [] -> Just p
            _ -> Nothing

(/) :: Ord err =>
    Grab err Form ->
    Dump err desideratum ->
    Grab err desideratum

(/) = (Grab./)


--- Simple form fields ---

text :: forall err.
    (Ord err, Err_Missing err, Err_Duplicate err) =>
    Grab err Text

text = here / Grab.dump f
  where
    f :: Form -> Extract err Text
    f (Form xs ctx) =
        case unique (map paramValue xs) of
            []         -> Grab.failure (ctx (Name []) .= err_missing)
            x : []     -> Grab.success x
            _ : _ : [] -> Grab.failure (ctx (Name []) .= err_duplicate)

optionalText :: forall err.
    (Ord err, Err_Duplicate err) =>
    Grab err (Maybe Text)

optionalText = here / Grab.dump f
  where
    f :: Form -> Extract err (Maybe Text)
    f (Form xs ctx) =
        case unique (map paramValue xs) of
            []         -> Grab.success Nothing
            x : []     -> Grab.success (Just x)
            _ : _ : [] -> Grab.failure (ctx (Name []) .= err_duplicate)

checkbox :: forall err.
    (Ord err, Err_OnlyAllowed err) =>
    Text ->
    Grab err Bool

checkbox yes = here / Grab.dump f
  where
    f :: Form -> Extract err Bool
    f (Form xs ctx) =
        case List.partition (== yes) (unique (map paramValue xs)) of
            ( []     , []    ) -> Grab.success False
            ( _ : [] , []    ) -> Grab.success True
            ( _      , _ : _ ) -> Grab.failure (ctx (Name []) .= err_onlyAllowed yes)


--- Internal ---

(.=) :: Ord err => Name -> err -> Log err
k .= err =
    coerce (Map.singleton k (Set.singleton err))

unique :: Ord a => [a] -> [a]
unique =
    Set.toList . Set.fromList

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe f = fix \r ->
    \case
        [] -> ([], [])
        x : xs ->
            let
              (bs, as) = r xs
            in
              case f x of
                Nothing -> (bs, x : as)
                Just y  -> (y : bs, as)

allJusts :: [Maybe a] -> Maybe [a]
allJusts = fix \r -> \case
    [] -> Just []
    (Nothing : _) -> Nothing
    (Just x : xs) -> (x :) <$> r xs


--- Dealing with unrecognized parameters ---

only :: forall err a. (Ord err, Err_Unexpected err) =>
    Grab err a -> Dump err a
only g =
    Grab.dump \i ->
        let
            r = Grab.runGrab i g
        in
            case Grab.residue r of
                Form [] _ -> Grab.discardResidue r
                Form xs ctx ->
                    Grab.extract
                        (Grab.log r
                            <> foldMap (\(Param n _) -> ctx n .= err_unexpected) xs)
                        (Grab.desideratum r)

etAlia :: Grab err a -> Dump err a
etAlia = Grab.discardResidue


--- Lists ---

natListWithIndex :: forall err a. Ord err =>
    Dump err a ->
    Grab err [(Natural, a)]

natListWithIndex =
  \d ->
    Grab.partition selectNats
    Grab./
    Grab.dump \(xs, ctx) ->
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

            results :: [(Natural, Extract err a)]
            results = map (\(n, f) -> (n, Grab.runDump f d)) forms
        in
            Grab.extract
                (foldMap (\(_, r) -> Grab.log r) results)
                (allJusts (
                    map
                        (\(n, r) ->
                            case Grab.desideratum r of
                                Nothing -> Nothing
                                Just v -> Just (n, v)
                        )
                        results
                ))

  where
    selectNats :: Form -> (([(Natural, Param)], Name -> Name), Form)
    selectNats (Form xs ctx) =
        let
            (s, r) = partitionMaybe f xs
        in
            ((s, ctx), Form r ctx)
      where
        f :: Param -> Maybe (Natural, Param)
        f (Param (Name (NameNat n : ns)) v) = Just (n, Param (Name ns) v)
        f _ = Nothing

natList :: Ord err =>
    Dump err a ->
    Grab err [a]

natList d =
    (map snd) <$> natListWithIndex d


--- Running grabs ---

grabParams :: Grab err a -> [Param] -> ([Param], Log err, Maybe a)
grabParams g xs =
    let
        r = Grab.runGrab (Form xs id) g
        rem = Grab.residue r
    in
        (f (Grab.residue r), Grab.log r, Grab.desideratum r)

  where
    f x = map (\(Param n v) -> Param (formContext x n) v) (formParams x)

dumpParams :: Dump err a -> [Param] -> (Log err, Maybe a)
dumpParams d xs =
    let
        r = Grab.runDump (Form xs id) d
    in
        (Grab.log r, Grab.desideratum r)
