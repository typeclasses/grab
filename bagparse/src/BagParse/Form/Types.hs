module BagParse.Form.Types where

import qualified BagParse.List.Types

import Data.List.NonEmpty (NonEmpty (..))

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

data StandardProblem
  = Missing
  | Duplicate
  | OnlyAllowed Text

standardProblemText :: StandardProblem -> Text
standardProblemText =
  \case
    Missing -> "Required parameter is missing."
    Duplicate -> "Parameter may not appear more than once."
    OnlyAllowed value -> "The only allowed value is `" <> value <> "`."

class Problem a where fromStandardProblem :: StandardProblem -> a

instance Problem StandardProblem where fromStandardProblem = id
instance Problem () where fromStandardProblem = const ()
instance Problem (Either StandardProblem a) where fromStandardProblem = Left
