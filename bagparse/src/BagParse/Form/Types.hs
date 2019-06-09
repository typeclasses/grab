module BagParse.Form.Types
  (

    Parser
  , Result
  , Harvest
  , Form
  , Param (..)
  , Name (..)
  , NamePart (..)
  , Log (..)
  , EnglishSentence (..)

  -- * Error classes
  , Err_Missing (..)
  , Err_Duplicate (..)
  , Err_Unexpected (..)
  , Err_OnlyAllowed (..)

  ) where

import qualified BagParse.List.Types

import Data.Coerce
import Data.String

import Numeric.Natural (Natural)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as Text


--- Parser ---

type Parser err a =
    BagParse.List.Types.Parser Param (Log err) a

type Result err a =
    BagParse.List.Types.Result Param (Log err) a

type Harvest err a =
    BagParse.List.Types.Harvest (Log err) a


--- Input ---

-- | The input to a 'Parser'.
type Form = [Param]

data Param =
  Param
    { paramName  :: Name
    , paramValue :: Text
    }

newtype Name = Name [NamePart]

data NamePart
  = NameStr Text
  | NameNat Natural


--- Log ---

newtype Log a =
  Log
    (Map Name (Set a))


--- Log monoid ---

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


--- Error classes ---

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

newtype EnglishSentence = EnglishSentence Text

deriving newtype instance IsString EnglishSentence

instance Err_Missing EnglishSentence where err_missing = "Required parameter is missing."
instance Err_Duplicate EnglishSentence where err_duplicate = "Parameter may not appear more than once."
instance Err_Unexpected EnglishSentence where err_unexpected = "Unexpected parameter."
instance Err_OnlyAllowed EnglishSentence where err_onlyAllowed value = EnglishSentence ("The only allowed value is `" <> value <> "`.")

instance Err_Missing () where err_missing = ()
instance Err_Duplicate () where err_duplicate = ()
instance Err_Unexpected () where err_unexpected = ()
instance Err_OnlyAllowed () where err_onlyAllowed = const ()


--- Eq and Ord instances ---

deriving stock instance Eq a => Eq (Log a)

deriving stock instance Eq NamePart
deriving stock instance Ord NamePart

deriving stock instance Eq Name
deriving stock instance Ord Name

deriving stock instance Eq Param
deriving stock instance Ord Param

deriving stock instance Eq EnglishSentence
deriving stock instance Ord EnglishSentence
