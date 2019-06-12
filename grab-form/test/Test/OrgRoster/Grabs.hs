{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE

    ApplicativeDo, BlockArguments, LambdaCase,
    OverloadedStrings, ScopedTypeVariables, ViewPatterns

#-}

module Test.OrgRoster.Grabs where

import Test.OrgRoster.Concepts

import qualified Data.GrabForm as Grab
import Data.GrabForm (only, natList, at, checkbox, text, optionalText, natListWithIndex)

import Data.Bifunctor

import qualified Data.Map as Map
import Data.Map (Map)

type Error = Grab.EnglishSentence
type Grab desideratum = Grab.Grab Error desideratum
type Log = Grab.Log Error
type Dump desideratum = Grab.Dump Error desideratum

roster :: Grab Roster
roster =
    Roster
        <$> at "org" (only org)
        <*> at "members" (only memberList)

org :: Grab OrgId
org = fmap OrgId text

memberList :: Grab MemberList
memberList =
    MemberList
        <$> at "existing" (only existingList)
        <*> at "new" (only (natList (only member)))

existingList :: Grab (Map RosterOrdinal Modification)
existingList =
    fmap (Map.fromList . map (first RosterOrdinal))
    (natListWithIndex (only modification))

modification :: Grab Modification
modification =
  do
    m <- member
    r <- at "remove" (only (checkbox "yes"))
    return (if r then Modification_Delete else Modification_Update m)

member :: Grab Member
member =
    Member
        <$> at "name"      (only (fmap (fmap OrgMemberName) optionalText))
        <*> at "isManager" (only (fmap isManagerRole (checkbox "yes")))
        <*> at "isUser"    (only (fmap isUserAccess (checkbox "yes")))

isManagerRole :: Bool -> OrgRole
isManagerRole =
    \case
        False -> OrgRole_Normal
        True -> OrgRole_Manager

isUserAccess :: Bool -> OrgContentAccess
isUserAccess =
    \case
        False -> OrgContentAccess_No
        True -> OrgContentAccess_Yes
