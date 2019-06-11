module BagParse.Form.Name
  ( Name (..), NamePart (..), showName, readName
  ) where

import Data.Coerce (coerce)
import Data.String (IsString (fromString))

import Numeric.Natural (Natural)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

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
