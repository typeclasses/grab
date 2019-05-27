module BagParse.Examples.OrgRoster where

import BagParse.Core
import BagParse.Prelude

import Data.Coerce
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Numeric.Natural

type FormParam = (Text, Text)

newtype OrgId = OrgId Text

newtype OrgMemberName = OrgMemberName Text

-- | A position in an org's member list.

newtype RosterOrdinal = RosterOrdinal Natural

-- | Whether an org member has permissions to administer the org.

data OrgRole
  = OrgRole_Normal   -- ^ No, a regular user with no management permission
  | OrgRole_Manager  -- ^ Yes, this user is an org manager

-- | Whether an org member gets access to Type Classes content.

data OrgContentAccess
  = OrgContentAccess_Yes  -- ^ Yes, has access to content
  | OrgContentAccess_No   -- ^ No, the membership does not grant content access

data Form =
  Form
    { form_org :: OrgId
    , form_members :: MemberList
    }

data MemberList =
  MemberList
    { members_existing :: [Existing]
    , members_new :: [Member]
    }

data Existing =
  Existing
    { existing_ordinal :: RosterOrdinal
    , existing_mod :: Modification
    }

data Modification
  = Modification_Update Member
  | Modification_Delete

data Member =
  Member
    { member_name :: Maybe OrgMemberName
    , member_role :: OrgRole
    , member_access :: OrgContentAccess
    }

data Error = Error Text

readForm :: [FormParam] -> Either (NonEmpty Error) Form
readForm = parseResultEither . parse formP

formP :: Parser [] FormParam (NonEmpty Error) Form
formP =
  do
    members <- memberListP
    org <- orgP
    return Form { form_org = org, form_members = members }

orgP :: Parser [] FormParam (NonEmpty Error) OrgId
orgP =
    fmap (\(k, v) -> OrgId v) $
    key "org" *> one (Error "Wrong number of org parameters" :| [])

memberListP :: Parser [] FormParam (NonEmpty Error) MemberList
memberListP = prefix "members."
  do
    existing <- existingListP
    new <- newListP
    nothing'
    return MemberList{ members_existing = existing, members_new = new }

existingListP :: Parser [] FormParam (NonEmpty Error) [Existing]
existingListP = prefix "existing." (bracketList '[' ']' (\x -> existingP x <* nothing'))

newListP :: Parser [] FormParam (NonEmpty Error) [Member]
newListP = prefix "new." (bracketList '[' ']' (const (memberP <* nothing')))

existingP :: Text -> Parser [] FormParam (NonEmpty Error) Existing
existingP x =
  do
    o <- coerce @Natural @RosterOrdinal <$> trivialP_either (readNat x)
    m <- modificationP
    return Existing { existing_ordinal = o, existing_mod = m }

modificationP :: Parser [] FormParam (NonEmpty Error) Modification
modificationP =
  do
    m <- memberP
    r <- (_ :: Parser [] FormParam (NonEmpty Error) Bool)
    return (if r then Modification_Delete else Modification_Update m)

memberP :: Parser [] FormParam (NonEmpty Error) Member
memberP = _

bracketList :: Char -> Char -> (Text -> Parser [] FormParam e a) -> Parser [] FormParam e [a]
bracketList a b f = _

readNat :: Text -> Either (NonEmpty Error) Natural
readNat = _

prefix :: Text -> Parser [] FormParam err a -> Parser [] FormParam e a
prefix x p = _

key :: Text -> Parser [] FormParam err [Text]
key k = _

nothing' :: Parser bag item (NonEmpty Error) ()
nothing' = nothing (Error "Unrecognized field" :| []) ()
