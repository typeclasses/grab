module Test.OrgRoster.Concepts where

import Numeric.Natural (Natural)

import Data.Map (Map)

import Data.Text (Text)

newtype OrgId = OrgId Text
    deriving (Eq, Show)

newtype OrgMemberName = OrgMemberName Text
    deriving (Eq, Show)

-- | A position in an org's member list.

newtype RosterOrdinal = RosterOrdinal Natural
    deriving (Show, Eq, Ord)

-- | Whether an org member has permissions to administer the org.

data OrgRole
  = OrgRole_Normal   -- ^ No, a regular user with no management permission
  | OrgRole_Manager  -- ^ Yes, this user is an org manager
    deriving (Eq, Show)

-- | Whether an org member gets access to Type Classes content.

data OrgContentAccess
  = OrgContentAccess_Yes  -- ^ Yes, has access to content
  | OrgContentAccess_No   -- ^ No, the membership does not grant content access
    deriving (Eq, Show)

data Roster =
  Roster
    { roster_org :: OrgId
    , roster_members :: MemberList
    }
    deriving (Eq, Show)

data MemberList =
  MemberList
    { members_existing :: Map RosterOrdinal Modification
    , members_new :: [Member]
    }
    deriving (Eq, Show)

data Modification
  = Modification_Update Member
  | Modification_Delete
    deriving (Eq, Show)

data Member =
  Member
    { member_name :: Maybe OrgMemberName
    , member_role :: OrgRole
    , member_access :: OrgContentAccess
    }
    deriving (Eq, Show)
