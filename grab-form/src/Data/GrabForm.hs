{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE

    BlockArguments, DerivingStrategies, FlexibleInstances,
    GeneralizedNewtypeDeriving, LambdaCase, OverloadedStrings,
    ScopedTypeVariables, StandaloneDeriving, TupleSections,
    TypeApplications, ViewPatterns

#-}

module Data.GrabForm
  (
  -- * Tutorial
  -- $tutorial

  -- * What is a form
  -- ** The Parameter type
    Param (..)
  -- ** The Name type
  , Name (..), NamePart (..), showName, readName
  -- ** The Form type
  , Form (..)

  -- * Error messages
  -- ** The Log type
  , Log (..)
  -- ** Error classes
  , Err_Missing (..), Err_Duplicate (..)
  , Err_Unexpected (..), Err_OnlyAllowed (..)
  -- ** English sentences as error messages
  , EnglishSentence (..), englishSentenceLogText

  -- * Grabbing data from forms
  -- ** Types: Grab and Dump
  , Grab, Dump
  -- ** Parameter name selection
  , at
  -- ** Simple form fields
  , text, optionalText, checkbox
  -- ** Lists
  , natList, natListWithIndex
  -- ** Dealing with unrecognized parameters
  , only, etAlia, remainder
  -- ** Applying a grab to a form
  , readTextParams

  ) where

import Prelude hiding ((/))

import Data.Coerce (coerce)
import Data.Function (fix)
import Data.String (IsString (fromString))
import Data.Traversable (for)

import Numeric.Natural (Natural)

import qualified Data.List as List

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Control.Grab as Grab
import Control.Grab ((/))


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

instance IsString Name
  where
    fromString = readName . Text.pack

data NamePart
  = NameStr Text
  | NameNat Natural
  | NameErr Text

instance IsString NamePart
  where
    fromString = NameStr . Text.pack

showName :: Name -> Text
showName (Name []) = ""
showName (Name (x : xs)) = showNamePart x <> showNameRemainder xs
  where
    showNameRemainder [] = ""
    showNameRemainder (y : ys) = showNamePart' y <> showNameRemainder ys

    showNamePart (NameStr s) = s
    showNamePart (NameNat n) = showNat n
    showNamePart (NameErr s) = s

    showNamePart' (NameStr s) = "." <> s
    showNamePart' (NameNat n) = showNat n
    showNamePart' (NameErr s) = s

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

newtype Log err =
  Log
    (Set (Name, err))

deriving newtype instance Ord err => Semigroup (Log err)
deriving newtype instance Ord err => Monoid (Log err)

deriving stock instance Eq err => Eq (Log err)
deriving stock instance Show err => Show (Log err)

(.=) :: Ord err => Name -> err -> Log err
k .= err =
    coerce (Set.singleton (k, err))


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
deriving newtype instance Show EnglishSentence

deriving stock instance Eq EnglishSentence
deriving stock instance Ord EnglishSentence

instance Err_Missing EnglishSentence where err_missing = "Required parameter is missing."
instance Err_Duplicate EnglishSentence where err_duplicate = "Parameter may not appear more than once."
instance Err_Unexpected EnglishSentence where err_unexpected = "Unexpected parameter."
instance Err_OnlyAllowed EnglishSentence where err_onlyAllowed value = EnglishSentence ("The only allowed value is `" <> value <> "`.")

englishSentenceLogText :: Log EnglishSentence -> Text

englishSentenceLogText =
    Text.unlines . map (uncurry f) . Set.toList . coerce
  where
    f :: Name -> EnglishSentence -> Text
    f name err = Text.concat
        [ showName name
        , ": "
        , coerce @EnglishSentence @Text err
        ]


--- Grab types ---

type Grab err a =
    Grab.Simple Form (Log err) a

type Dump err a =
    Grab.Dump Form (Log err) a

type Extract err a =
    Grab.Extract (Log err) a


--- Parameter name selection ---

atGrab :: Ord err => NamePart -> Grab err Form
atGrab k =
    Grab.partition \(Form xs ctx) ->
        let
            (s, r) = partitionMaybe (namePrefixPartition k) xs
        in
            (Form s (ctx . coerce (k :)), Form r ctx)

at :: Ord err => NamePart -> Dump err a -> Grab err a
at k d = atGrab k / d

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

herePartition :: Param -> Maybe Param
herePartition =
    \case
        p@(Param (Name []) _) -> Just p
        _ -> Nothing


--- Simple form fields ---

text :: forall err.
    (Ord err, Err_Missing err, Err_Duplicate err) =>
    Grab err Text

text = here / Grab.dump f
  where
    f :: Form -> Extract err Text
    f (Form xs ctx) =
        case unique (map paramValue xs) of
            []        -> Grab.failure (ctx (Name []) .= err_missing)
            x : []    -> Grab.success x
            _ : _ : _ -> Grab.failure (ctx (Name []) .= err_duplicate)

optionalText :: forall err.
    (Ord err, Err_Duplicate err) =>
    Grab err (Maybe Text)

optionalText = here / Grab.dump f
  where
    f :: Form -> Extract err (Maybe Text)
    f (Form xs ctx) =
        case unique (map paramValue xs) of
            []        -> Grab.success Nothing
            x : []    -> Grab.success (Just x)
            _ : _ : _ -> Grab.failure (ctx (Name []) .= err_duplicate)

checkbox :: forall err.
    (Ord err, Err_OnlyAllowed err) =>
    Text ->
    Grab err Bool

checkbox yes = here / Grab.dump f
  where
    f :: Form -> Extract err Bool
    f (Form xs ctx) =
        case List.partition (== yes) (unique (map paramValue xs)) of
            ( []    , []    ) -> Grab.success False
            ( _ : _ , []    ) -> Grab.success True
            ( _     , _ : _ ) -> Grab.failure (ctx (Name []) .= err_onlyAllowed yes)


--- Internal ---

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


--- Dealing with unrecognized parameters ---

only :: forall err a. (Ord err, Err_Unexpected err) =>
    Grab err a -> Dump err a
only g =
    Grab.dump \i ->
        let
            r = Grab.runGrab g i
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

remainder :: Ord err => Grab err [Param]
remainder = Grab.partition \(Form xs _) -> (xs, Form [] id)


--- Lists ---

groupByFst :: Ord a => [(a, b)] -> [(a, [b])]
groupByFst =
    Map.toList .
    foldr (Map.unionWith (++)) Map.empty .
    map (\(a, b) -> Map.singleton a [b])

natListWithIndex :: forall err a. Ord err =>
    Dump err a ->
    Grab err [(Natural, a)]

natListWithIndex =
  \d ->
    Grab.partition selectNats
    /
    Grab.dump \(xs, ctx) ->
        for (groupByFst xs) \(n, xs') ->
            Grab.runDump
                (fmap (n,) d)
                (Form xs' (ctx . coerce (NameNat n :)))

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


--- Applying a grab to a form ---

readTextParams :: Ord err => Dump err a -> [(Text, Text)] -> (Log err, Maybe a)
readTextParams d x =
     let
         r = Grab.runDump d (textParamsToForm x)
     in
         (Grab.log r, Grab.desideratum r)

textParamsToForm :: [(Text, Text)] -> Form
textParamsToForm xs = Form (map textParam xs) id

textParam :: (Text, Text) -> Param
textParam (x, y) = Param (readName x) y


{- $tutorial

We are concerned here with data submitted by web browsers in a normal form
submission. Ignoring the encoding details, we can think of a form as looking
something like this:

> name:               Alonzo
> state:              Montana
> security_question:  What is your favorite hobby?
> security_answer:    watching cars

This example has four parameters. Each parameter has a name and a value. We
might represent this in Haskell as:

> [ ("name", "Alonzo")
> , ("state", "Montana")
> , ("security_question", "What is your favorite hobby?")
> , ("security_answer", "watching cars")
> ]

Suppose we're only interested in two parts of this form: The name and the state.

> nameAndState :: Grab EnglishSentence (Text, Text)
> nameAndState =
>     (,)
>         <$> at "name" (only text)
>         <*> at "state" (only text)

If we apply @nameAndState@ to the form parameters above, we get the following
result: @(\"Alonzo\", \"Montana\")@

> λ> :{
>  > readTextParams (etAlia nameAndState)
>  >     [ ("name", "Alonzo")
>  >     , ("state", "Montana")
>  >     , ("security_question", "What is your favorite hobby?")
>  >     , ("security_answer", "watching cars")
>  >     ]
>  > :}
> ( Log []
> , Just ("Alonzo", "Montana")
> )

When receiving information submitted from an external source, there is usually
some possibility that the input is invalid. Consider the following form that is
missing the "state" field. In this case, the result we get is @Nothing@,
accompanied by an error message indicating that something is missing.

> λ> :{
>  > readTextParams (etAlia nameAndState)
>  >     [ ("name", "Alonzo")
>  >     , ("security_question", "What is your favorite hobby?")
>  >     , ("security_answer", "watching cars")
>  >     ]
>  > :}
> ( Log [("state", "Required parameter is missing.")]
> , Nothing
> )

The 'etAlia' function we've been using signifies that the input is allowed to
contain parameters other than the ones that @nameAndState@ grabs. If we use
'only' instead, we can specify that there should be no additional parameters.

> λ> :{
>  > readTextParams (only nameAndState)
>  >     [ ("name", "Alonzo")
>  >     , ("state", "Montana")
>  >     , ("security_question", "What is your favorite hobby?")
>  >     , ("security_answer", "watching cars")
>  >     ]
>  > :}
> ( Log [ ("security_question", "Unexpected parameter.")
>       , ("security_answer", "Unexpected parameter.")
>       ]
> , Just ("Alonzo", "Montana")
> )

However, we still get the result: @(\"Alonzo\", \"Montana\")@. Unexpected parameters
do not prevent us from being able to read the form. Whether you choose 'only' or
'etAlia' only determines whether these warnings end up in the log; it does not
affect whether reading the form succeeds or fails.

Duplicate parameters are not permitted, since we cannot know which of the values
to accept as the real one. Alonzo cannot live in both Georgia and Montana:

> λ> :{
>  > readTextParams (only nameAndState)
>  >     [ ("name", "Alonzo")
>  >     , ("state", "Georgia")
>  >     , ("state", "Montana")
>  >     ]
>  > :}
> ( Log [("state", "Parameter may not appear more than once.")]
> , Nothing
> )

Duplicated parameters are only allowed if they have the same value, because in
that case the problem of deciding which value to accept does not arise.

> λ> :{
>  > readTextParams (only nameAndState)
>  >     [ ("name", "Alonzo")
>  >     , ("state", "Montana")
>  >     , ("state", "Montana")
>  >     ]
>  > :}
> ( Log []
> , Just ("Alonzo", "Montana")
> )

Sometimes a form has a tree structure. Suppose there are multiple security
questions. If we were using a data format like YAML, it might look like this:

> name: Alonzo
> state: Montana
> security:
>   - Q: What is your favorite hobby?
>     A: watching cars
>   - Q: What is your oldest sibling's name?
>     A: melman
>   - Q: What was the make and model of your first car?
>     A: bmw x5

To cajole this data into our concept of a form as a list of parameters, we need
to flatten it somehow. We adopt the following convention:

> name:           Alonzo
> state:          Montana
> security[1].Q:  What is your favorite hobby?
> security[1].A:  watching cars
> security[2].Q:  What is your oldest sibling's name?
> security[2].A:  melman
> security[3].Q:  What was the make and model of your first car?
> security[3].A:  bmw x5

Let's define a data type to represent a question and answer:

> data QA = QA { qa_question :: Text, qa_answer :: Text } deriving (Eq, Show)

> nameStateAndQAs :: Grab EnglishSentence (Text, Text, [QA])
> nameStateAndQAs =
>     (,,)
>         <$> at "name" (only text)
>         <*> at "state" (only text)
>         <*> at "security" (only (natList (only qa)))

> qa :: Grab EnglishSentence QA
> qa =
>     QA
>         <$> at "Q" (only text)
>         <*> at "A" (only text)

> λ> :{
>  > readTextParams (only nameStateAndQAs)
>  >     [ ("name", "Alonzo")
>  >     , ("state", "Montana")
>  >     , ("security[0].Q", "What is your favorite hobby?")
>  >     , ("security[0].A", "watching cars")
>  >     , ("security[1].Q", "What is your oldest sibling's name?")
>  >     , ("security[1].A", "melman")
>  >     , ("security[2].Q", "What was the make and model of your first car?")
>  >     , ("security[2].A", "bmw x5")
>  >     ]
>  > :}
> ( Log []
> , Just
>       ( "Alonzo"
>       , "Montana"
>       , [ QA
>             { qa_question = "What is your favorite hobby?"
>             , qa_answer = "watching cars"
>             }
>         , QA
>             { qa_question = "What is your oldest sibling's name?"
>             , qa_answer = "melman"
>             }
>         , QA
>             { qa_question = "What was the make and model of your first car?"
>             , qa_answer = "bmw x5"
>             }
>         ]
>       )
> )

The parameters of the list may appear in any order. The order of the result is
determined by the numbers in the parameter names.

> λ> :{
>  > readTextParams (only (at "security" (only (natList (only qa)))))
>  >     [ ("security[2].Q", "What was the make and model of your first car?")
>  >     , ("security[1].A", "melman")
>  >     , ("security[0].Q", "What is your favorite hobby?")
>  >     , ("security[1].Q", "What is your oldest sibling's name?")
>  >     , ("security[0].A", "watching cars")
>  >     , ("security[2].A", "bmw x5")
>  >     ]
>  > :}
> ( Log []
> , Just
>       [ QA
>           { qa_question = "What is your favorite hobby?"
>           , qa_answer = "watching cars"
>           }
>       , QA
>           { qa_question = "What is your oldest sibling's name?"
>           , qa_answer = "melman"
>           }
>       , QA
>           { qa_question = "What was the make and model of your first car?"
>           , qa_answer = "bmw x5"
>           }
>       ]
> )

Error messages work the same within nested grabs. The result is a complete list
of every error encountered.

> λ> :{
>  > readTextParams (only nameStateAndQAs)
>  >     [ ("state", "Montana")
>  >     , ("itchy face", "yes")
>  >     , ("security[0].Q", "What is your favorite hobby?")
>  >     , ("security[0].A", "watching cars")
>  >     , ("security[1].Q", "What is your oldest sibling's name?")
>  >     , ("security[1].A", "melman")
>  >     , ("security[1].A", "iowa")
>  >     , ("security[2].Q", "What was the make and model of your first car?")
>  >     , ("security[2].A", "bmw x5")
>  >     , ("security[2].A2", "xyz")
>  >     ]
>  > :}
> ( Log [ ("name", "Required parameter is missing.")
>       , ("itchy face", "Unexpected parameter.")
>       , ("security[1].A", "Parameter may not appear more than once.")
>       , ("security[2].A2", "Unexpected parameter.")
>       ]
> , Nothing
> )

-}
