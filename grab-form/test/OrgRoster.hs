import qualified BagParse.Form.Types as X
import qualified BagParse.Form.Name as X
import qualified BagParse.Form.Log as X
import qualified BagParse.Form.Prelude as X

import BagParse.Form.Input (Form (..), Param (..))
import BagParse.Form.Prelude

import Data.Bifunctor
import Data.Coerce
import Data.List.NonEmpty (NonEmpty (..))

import Control.Monad (when)

import Numeric.Natural

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import System.IO (hSetEncoding, stdout, stderr, utf8)
import System.Exit (exitFailure)

type Error = EnglishSentence
type Grab value = X.Grab Error value
type Log = X.Log Error
type Result value = X.Result Error value
type Dump value = X.Dump Error value
type Product value = X.Product Error value

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

roster :: Grab Roster
roster =
    Roster
        <$> (at "org" >-> only org)
        <*> (at "members" >-> only memberList)

org :: Grab OrgId
org = fmap OrgId text

memberList :: Grab MemberList
memberList =
    MemberList
        <$> (at "existing" >-> only existingList)
        <*> (at "new" >-> only (natList (only member)))

existingList :: Grab (Map RosterOrdinal Modification)
existingList =
    fmap (Map.fromList . map (first RosterOrdinal))
    (natListWithIndex (only modification))

modification :: Grab Modification
modification =
  do
    m <- member
    r <- at "remove" >-> only (checkbox "yes")
    return (if r then Modification_Delete else Modification_Update m)

member :: Grab Member
member =
    Member
        <$> (at "name" >-> only (fmap (fmap OrgMemberName) optionalText))
        <*> (at "isManager" >-> only (fmap isManagerRole (checkbox "yes")))
        <*> (at "isUser" >-> only (fmap isUserAccess (checkbox "yes")))

isManagerRole :: Bool -> OrgRole
isManagerRole = \case False -> OrgRole_Normal; True -> OrgRole_Manager

isUserAccess :: Bool -> OrgContentAccess
isUserAccess = \case False -> OrgContentAccess_No; True -> OrgContentAccess_Yes

tests :: IO Bool
tests =
  checkParallel $$(discover)

main :: IO ()
main =
  do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    ok <- tests
    when (not ok) exitFailure

prop_1 :: Property
prop_1 = example
  Example
    { ex_grab = at "org" >-> only org
    , ex_params = [("org", "13f499c3")]
    , ex_remainder = []
    , ex_log = []
    , ex_value = Just (OrgId "13f499c3")
    }

prop_2 :: Property
prop_2 = example
  Example
    { ex_grab = at "members" >-> only memberList
    , ex_params = [("org", "13f499c3")]
    , ex_remainder = [("org", "13f499c3")]
    , ex_log = []
    , ex_value = Just (MemberList mempty mempty)
    }

prop_3 :: Property
prop_3 = example
  Example
    { ex_grab = member
    , ex_params = [("name", "chris")]
    , ex_remainder = []
    , ex_log = []
    , ex_value = Just (Member (Just (OrgMemberName "chris")) OrgRole_Normal OrgContentAccess_No)
    }

prop_4 :: Property
prop_4 = example
  Example
    { ex_grab = member
    , ex_params = [("name", "chris"), ("isManager", "yes")]
    , ex_remainder = []
    , ex_log = []
    , ex_value = Just (Member (Just (OrgMemberName "chris")) OrgRole_Manager OrgContentAccess_No)
    }

prop_5 :: Property
prop_5 = example
  Example
    { ex_grab = member
    , ex_params = [("name", "chris"), ("isManager", "huh")]
    , ex_remainder = []
    , ex_log = ["isManager: The only allowed value is `yes`."]
    , ex_value = Nothing
    }

prop_6 :: Property
prop_6 = example
  Example
    { ex_grab = at "org" >-> only org
    , ex_params = []
    , ex_remainder = []
    , ex_log = ["org: Required parameter is missing."]
    , ex_value = Nothing
    }

prop_7 :: Property
prop_7 = example
  Example
    { ex_grab = (,) <$> (at "org1" >-> only org)
                    <*> (at "org2" >-> only org)
    , ex_params = [("org1", "abc"), ("org3", "xyz"), ("org2", "def")]
    , ex_remainder = [("org3", "xyz")]
    , ex_log = []
    , ex_value = Just (OrgId "abc", OrgId "def")
    }

prop_8 :: Property
prop_8 = example
  Example
    { ex_grab = (,) <$> (at "org1" >-> only org)
                    <*> (at "org2" >-> only org)
    , ex_params = [("org1", "abc"), ("org2", "xyz"), ("org2", "def"), ("org3", "jkl")]
    , ex_remainder = [("org3", "jkl")]
    , ex_log = ["org2: Parameter may not appear more than once."]
    , ex_value = Nothing
    }

prop_9 :: Property
prop_9 = example
  Example
    { ex_grab = (,) <$> (at "org1" >-> only org)
                    <*> (at "org2" >-> only org)
    , ex_params = [("org1", "abc"), ("org3", "jkl")]
    , ex_remainder = [("org3", "jkl")]
    , ex_log = ["org2: Required parameter is missing."]
    , ex_value = Nothing
    }

prop_10 :: Property
prop_10 = example
  Example
    { ex_grab = (,) <$> (at "org1" >-> only org)
                    <*> (at "org2" >-> only org)
    , ex_params = [("org1", "abc"), ("org1", "def"), ("org3", "jkl")]
    , ex_remainder = [("org3", "jkl")]
    , ex_log = ["org1: Parameter may not appear more than once.",
                "org2: Required parameter is missing."]
    , ex_value = Nothing
    }

prop_11 :: Property
prop_11 = example
  Example
    { ex_grab = at "org" >-> only org
    , ex_params = [("org", "abc"), ("org", "abc")]
    , ex_remainder = []
    , ex_log = []
    , ex_value = Just (OrgId "abc")
    }

prop_12 :: Property
prop_12 = example
  Example
    { ex_grab = roster
    , ex_params =
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
    , ex_remainder = [("csrfToken", "bf016ab")]
    , ex_log = []
    , ex_value = Just Roster
        { roster_org = OrgId "13f499c3"
        , roster_members = MemberList
            { members_existing =
                Map.singleton (RosterOrdinal 1) (Modification_Update (Member (Just (OrgMemberName "Broccoli Rob")) OrgRole_Manager OrgContentAccess_No)) <>
                Map.singleton (RosterOrdinal 2) (Modification_Update (Member (Just (OrgMemberName "Jingle Jangle")) OrgRole_Normal OrgContentAccess_Yes)) <>
                Map.singleton (RosterOrdinal 4) Modification_Delete <>
                Map.singleton (RosterOrdinal 7) (Modification_Update (Member (Just (OrgMemberName "Hopscotch")) OrgRole_Normal OrgContentAccess_Yes))
            , members_new =
                Member (Just (OrgMemberName "Lunchbox")) OrgRole_Normal OrgContentAccess_No :
                Member (Just (OrgMemberName "")) OrgRole_Manager OrgContentAccess_No :
                []
            }
        }
    }

prop_13 :: Property
prop_13 = example
  Example
    { ex_grab = at "new" >-> only (natList (only member))
    , ex_params =
        [ ("new[1].name", "Lunchbox")
        , ("new[2].isManager", "yes")
        , ("new[2].name", "")
        ]
    , ex_remainder = []
    , ex_log = []
    , ex_value = Just $
        Member (Just (OrgMemberName "Lunchbox")) OrgRole_Normal OrgContentAccess_No :
        Member (Just (OrgMemberName "")) OrgRole_Manager OrgContentAccess_No :
        []
    }

data Example a =
  Example
    { ex_grab :: Grab a
    , ex_params :: [(Text, Text)]
    , ex_remainder :: [(Text, Text)]  -- ^ Expected remainder
    , ex_log :: [Text]                -- ^ Expected log
    , ex_value :: Maybe a             -- ^ Expected result value
    }

example :: (Eq a, Show a) => Example a -> Property
example (Example a xs r l mv) =
    withTests 1 $ property
      do
        let
            params = map (\(k, v) -> Param (readName k) v) xs
            rem = map (\(k, v) -> Param (readName k) v) r
            form = Form params id
            res = run form a

        formParams (X.toRemainder res) === rem
        englishSentenceLogText (X.toLog res) === Text.unlines l
        X.toValueMaybe res === mv
