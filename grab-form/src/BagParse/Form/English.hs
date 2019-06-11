module BagParse.Form.English
  ( EnglishSentence (..)
  , englishSentenceLogText
  ) where

import BagParse.Form.Errors
import BagParse.Form.Log
import BagParse.Form.Name
import BagParse.Form.Types

import Data.Coerce (coerce)

import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.String (IsString)

import Data.Text (Text)
import qualified Data.Text as Text

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
