module BagParse.Form.Prelude (checkbox, natMap, natList, key) where

import BagParse.Form.Types
import BagParse.List.Prelude

import Numeric.Natural

import Data.Map (Map)

import Data.List.NonEmpty (NonEmpty (..))

import Data.Text (Text)
import qualified Data.Text as Text

failure :: err -> Either (FormError err) a
failure x = Left (ParamError Nothing x :| [])

success :: a -> Either e a
success = Right

checkbox
    :: (Err_Missing err, Err_Duplicate err, Err_OnlyAllowed err)
    => Text -> Parser err Bool
checkbox yes = dump \case
    []                                   ->  success False
    [FormParam k v] | k == "", v == yes  ->  success True
                    | k == ""            ->  failure (err_onlyAllowed yes)
    _ : _ : _                            ->  failure err_duplicate

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
