module BagParse.Examples.OrgRoster where

import qualified BagParse.Form.Types

import BagParse.Form.Types (Param (..))
import BagParse.Form.Prelude
import BagParse.Parser.Prelude (parseEither)

import Data.Bifunctor
import Data.Coerce
import Data.List.NonEmpty (NonEmpty (..))
import Numeric.Natural

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

type Parser a = BagParse.Form.Types.Parser Error a

type Error = BagParse.Form.Types.EnglishSentence

type Log = BagParse.Form.Types.Log Error

newtype OrgId = OrgId Text
    deriving Show

newtype OrgMemberName = OrgMemberName Text
    deriving Show

-- | A position in an org's member list.

newtype RosterOrdinal = RosterOrdinal Natural
    deriving (Show, Eq, Ord)

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

readRoster :: [Param] -> Either Log Roster
readRoster = parseEither rosterP

rosterP :: Parser Roster
rosterP =
  do
    members <- memberListP
    org <- orgP
    return Roster { roster_org = org, roster_members = members }

orgP :: Parser OrgId
orgP =
    at "org" (coerce @Text @OrgId <$> text)

memberListP :: Parser MemberList
memberListP = (at "members" . only)
  do
    existing <-
        (Map.fromList . map (first (coerce @Natural @RosterOrdinal)) . Map.toList)
        <$> (at "existing" . only) (natMap (only modificationP))
    new <- (at "new" . only) (natList (only memberP))
    return MemberList{ members_existing = existing, members_new = new }

modificationP :: Parser Modification
modificationP =
  do
    m <- memberP
    r <- at "remove" (checkbox "yes")
    return (if r then Modification_Delete else Modification_Update m)

memberP :: Parser Member
memberP =
  do
    name <- fmap (coerce @Text @OrgMemberName) <$> at "name" optionalText
    role <-
        (\case False -> OrgRole_Normal; True -> OrgRole_Manager)
        <$> at "isManager" (checkbox "yes")
    access <-
        (\case False -> OrgContentAccess_No; True -> OrgContentAccess_Yes)
        <$> at "isUser" (checkbox "yes")
    return Member{ member_name = name, member_role = role, member_access = access }

test :: IO ()
test =
  do
    Just params <- traverse (\(k, v) -> Param <$> readName k <*> pure v) testInput
    case readRoster testInput of
      Left log -> Text.putStr (englishSentenceLogText log)
      Right x -> print x

testInput :: [(Text, Text)]
testInput =
  [ ("members.existing[1].name", "Broccoli Rob")
  , ("members.existing[1].isManager", "yes")
  , ("members.existing[2].name", "Jingle Jangle")
  , ("members.existing[2].isUser", "yes")
  , ("members.existing[4].name", "")
  , ("members.existing[4].isUser", "yes")
  , ("members.existing[4].remove", "yes")
  , ("members.existing[7].name", "Hopscotch")
  , ("members.existing[7].isUser", "yes")
  , ("members.new[1].name", "Lunchbox")
  , ("members.new[2].isManager", "yes")
  , ("members.new[2].name", "")
  , ("csrfToken", "bf016ab")
  , ("org", "13f499c3")
  ]
