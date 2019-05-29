module BagParse.Examples.OrgRoster where

import qualified BagParse.Form.Types

import BagParse.Form.Types (FormParam)
import BagParse.Form.Prelude
import BagParse.Parser.Prelude (parseEither)

import Data.Coerce
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Text (Text)
import Numeric.Natural

type Parser a = BagParse.Form.Types.Parser Error a

type FormError = BagParse.Form.Types.FormError Error

data Error = Error Text
    deriving Show

newtype OrgId = OrgId Text
    deriving Show

newtype OrgMemberName = OrgMemberName Text
    deriving Show

-- | A position in an org's member list.

newtype RosterOrdinal = RosterOrdinal Natural
    deriving Show

-- | Whether an org member has permissions to administer the org.

data OrgRole
  = OrgRole_Normal   -- ^ No, a regular user with no management permission
  | OrgRole_Manager  -- ^ Yes, this user is an org manager
    deriving Show

-- | Whether an org member gets access to Type Classes content.

data OrgContentAccess
  = OrgContentAccess_Yes  -- ^ Yes, has access to content
  | OrgContentAccess_No   -- ^ No, the membership does not grant content access
    deriving Show

data Roster =
  Roster
    { roster_org :: OrgId
    , roster_members :: MemberList
    }
  deriving Show

data MemberList =
  MemberList
    { members_existing :: Map RosterOrdinal Modification
    , members_new :: [Member]
    }
    deriving Show

data Modification
  = Modification_Update Member
  | Modification_Delete
    deriving Show

data Member =
  Member
    { member_name :: Maybe OrgMemberName
    , member_role :: OrgRole
    , member_access :: OrgContentAccess
    }
    deriving Show

readRoster :: [FormParam] -> Either FormError Roster
readRoster = parseEither rosterP

rosterP :: Parser Roster
rosterP =
  do
    members <- memberListP
    org <- orgP
    return Roster { roster_org = org, roster_members = members }

orgP :: Parser OrgId
orgP =
    fmap (\(k, v) -> OrgId v) $
    key "org" *> _ --one (Error "Wrong number of org parameters" :| [])

memberListP :: Parser MemberList
memberListP = prefix "members"
  do
    existing <- key "existing" (natMap (modificationP <* nothing') <* nothing')
    new <- key "new" (natList (memberP <* nothing') <* nothing')
    nothing'
    return MemberList{ members_existing = existing, members_new = new }

modificationP :: Parser Modification
modificationP =
  do
    m <- memberP
    r <- checkbox "remove" "yes"
    return (if r then Modification_Delete else Modification_Update m)

memberP :: Parser Member
memberP = _

nothing' :: Parser ()
nothing' = nothing (Error "Unrecognized field" :| []) ()

test :: IO ()
test = print (readRoster testInput)

testInput :: [FormParam]
testInput =
  [ FormParam "members.existing[1].name" "Broccoli Rob"
  , FormParam "members.existing[1].isManager" "yes"
  , FormParam "members.existing[2].name" "Jingle Jangle"
  , FormParam "members.existing[2].isUser" "yes"
  , FormParam "members.existing[4].name" ""
  , FormParam "members.existing[4].isUser" "yes"
  , FormParam "members.existing[4].remove" "yes"
  , FormParam "members.existing[7].name" "Hopscotch"
  , FormParam "members.existing[7].isUser" "yes"
  , FormParam "members.new[1].name" "Lunchbox"
  , FormParam "members.new[2].isManager" "yes"
  , FormParam "members.new[2].name" ""
  , FormParam "csrfToken" "bf016ab"
  , FormParam "org" "13f499c3"
  ]
