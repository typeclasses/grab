module BagParse.Form.Types where

import qualified BagParse.List.Types

import Data.List.NonEmpty (NonEmpty (..))

import Data.String

import Data.Text (Text)
import qualified Data.Text as Text

type Form = [FormParam]

data FormParam =
  FormParam
    { formParamName  :: Text
    , formParamValue :: Text
    }

type FormError a = NonEmpty (ParamError a)

data ParamError a =
  ParamError
    { errorPath :: Maybe Text
    , errorDescription :: a
    }

type Parser err a = BagParse.List.Types.Parser FormParam (FormError err) a

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

newtype ProblemText = ProblemText Text
    deriving newtype (IsString)

instance Err_Missing ProblemText where err_missing = "Required parameter is missing."
instance Err_Duplicate ProblemText where err_duplicate = "Parameter may not appear more than once."
instance Err_Unexpected ProblemText where err_unexpected = "Unexpected parameter."
instance Err_OnlyAllowed ProblemText where err_onlyAllowed value = ProblemText ("The only allowed value is `" <> value <> "`.")

instance Err_Missing () where err_missing = ()
instance Err_Duplicate () where err_duplicate = ()
instance Err_Unexpected () where err_unexpected = ()
instance Err_OnlyAllowed () where err_onlyAllowed = const ()
