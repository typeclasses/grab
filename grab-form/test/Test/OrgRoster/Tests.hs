{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

{-# LANGUAGE

    BlockArguments, LambdaCase, OverloadedStrings,
    ScopedTypeVariables, TemplateHaskell, ViewPatterns

#-}

module Test.OrgRoster.Tests where

import Test.OrgRoster.Concepts
import Test.OrgRoster.Grabs

import Data.GrabForm (only, etAlia, natList, readName, Name (..), NamePart (..), Param (..), at, remainder, englishSentenceLogText, readTextParams)

import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as Text

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

testGroup :: Group
testGroup = $$(discover)

prop_1 :: Property
prop_1 = example
  Example
    { ex_dump = only ((,) <$> at "org" (only org) <*> remainder)
    , ex_params = [("org", "13f499c3")]
    , ex_log = []
    , ex_value = Just (OrgId "13f499c3", [])
    }

prop_2 :: Property
prop_2 = example
  Example
    { ex_dump = etAlia (at "members" (only memberList))
    , ex_params = [("org", "13f499c3")]
    , ex_log = []
    , ex_value = Just (MemberList mempty mempty)
    }

prop_3 :: Property
prop_3 = example
  Example
    { ex_dump = only member
    , ex_params = [("name", "chris")]
    , ex_log = []
    , ex_value = Just (Member (Just (OrgMemberName "chris")) OrgRole_Normal OrgContentAccess_No)
    }

prop_4 :: Property
prop_4 = example
  Example
    { ex_dump = only member
    , ex_params = [("name", "chris"), ("isManager", "yes")]
    , ex_log = []
    , ex_value = Just (Member (Just (OrgMemberName "chris")) OrgRole_Manager OrgContentAccess_No)
    }

prop_5 :: Property
prop_5 = example
  Example
    { ex_dump = only member
    , ex_params = [("name", "chris"), ("isManager", "huh")]
    , ex_log = ["isManager: The only allowed value is `yes`."]
    , ex_value = Nothing
    }

prop_6 :: Property
prop_6 = example
  Example
    { ex_dump = only (at "org" (only org))
    , ex_params = []
    , ex_log = ["org: Required parameter is missing."]
    , ex_value = Nothing
    }

prop_7 :: Property
prop_7 = example
  Example
    { ex_dump = only ((,,) <$> at "org1" (only org)
                           <*> at "org2" (only org)
                           <*> remainder)
    , ex_params = [("org1", "abc"), ("org3", "xyz"), ("org2", "def")]
    , ex_log = []
    , ex_value = Just (OrgId "abc", OrgId "def", [Param (Name [NameStr "org3"]) "xyz"])
    }

prop_8 :: Property
prop_8 = example
  Example
    { ex_dump = etAlia ((,) <$> at "org1" (only org)
                            <*> at "org2" (only org))
    , ex_params = [("org1", "abc"), ("org2", "xyz"), ("org2", "def"), ("org3", "jkl")]
    , ex_log = ["org2: Parameter may not appear more than once."]
    , ex_value = Nothing
    }

prop_9 :: Property
prop_9 = example
  Example
    { ex_dump = etAlia ((,) <$> at "org1" (only org)
                            <*> at "org2" (only org))
    , ex_params = [("org1", "abc"), ("org3", "jkl")]
    , ex_log = ["org2: Required parameter is missing."]
    , ex_value = Nothing
    }

prop_10 :: Property
prop_10 = example
  Example
    { ex_dump = etAlia ((,) <$> at "org1" (only org)
                            <*> at "org2" (only org))
    , ex_params = [("org1", "abc"), ("org1", "def"), ("org3", "jkl")]
    , ex_log = ["org1: Parameter may not appear more than once.",
                "org2: Required parameter is missing."]
    , ex_value = Nothing
    }

prop_11 :: Property
prop_11 = example
  Example
    { ex_dump = only (at "org" (only org))
    , ex_params = [("org", "abc"), ("org", "abc")]
    , ex_log = []
    , ex_value = Just (OrgId "abc")
    }

prop_12 :: Property
prop_12 = example
  Example
    { ex_dump = etAlia roster
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
    { ex_dump = only (at "new" (only (natList (only member))))
    , ex_params =
        [ ("new[1].name", "Lunchbox")
        , ("new[2].isManager", "yes")
        , ("new[2].name", "")
        ]
    , ex_log = []
    , ex_value = Just $
        Member (Just (OrgMemberName "Lunchbox")) OrgRole_Normal OrgContentAccess_No :
        Member (Just (OrgMemberName "")) OrgRole_Manager OrgContentAccess_No :
        []
    }

data Example a =
  Example
    { ex_dump :: Dump a
    , ex_params :: [(Text, Text)]
    , ex_log :: [Text]              -- ^ Expected log
    , ex_value :: Maybe a           -- ^ Expected value
    }

example :: (Eq a, Show a) => Example a -> Property
example (Example a xs logLines expectedValue) =
    withTests 1 $ property
      do
        let
            expectedLog = Text.unlines logLines
            (englishSentenceLogText -> gotLog, gotValue) = readTextParams a xs

        (gotLog, gotValue) === (expectedLog, expectedValue)
