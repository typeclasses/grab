module BagParse.Form.Prelude (checkbox, natMap, natList, key) where

import BagParse.Form.Types
import BagParse.List.Prelude

import Numeric.Natural

import Data.Map (Map)

import Data.List.NonEmpty (NonEmpty (..))

import Data.Text (Text)
import qualified Data.Text as Text

(❌) :: Problem err => () -> StandardProblem -> Either (FormError err) a
() ❌ x = Left (ParamError Nothing (fromStandardProblem x) :| [])

(✅) :: () -> a -> Either e a
() ✅ x = Right x

checkbox :: Problem err => Text -> Parser err Bool
checkbox yes = dump \case
    []                                   ->  () ✅ False
    [FormParam k v] | k == "", v == yes  ->  () ✅ True
                    | k == ""            ->  () ❌ OnlyAllowed yes
    _ : _ : _                            ->  () ❌ Duplicate

natMap :: Parser err a -> Parser err (Map Natural a)
natMap = _

natList :: Parser err a -> Parser err [a]
natList = _

key :: Text -> Parser err a -> Parser err a
key k = _

{-

bracketList :: Char -> Char -> (Text -> Parser [FormParam] e a) -> Parser [FormParam] e [a]
bracketList a b f = _

readNat :: Text -> Either (NonEmpty Error) Natural
readNat = _

-}
