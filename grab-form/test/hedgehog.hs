{-# OPTIONS_GHC -Wall #-}

import qualified Test.OrgRoster.Tests
import qualified Test.Tutorial

import Control.Monad (when)
import Data.Monoid (All (All))

import System.IO (hSetEncoding, stdout, stderr, utf8)
import System.Exit (exitFailure)

import Hedgehog

main :: IO ()
main =
  do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    ok <- all id <$> traverse checkParallel testGroups
    when (not ok) exitFailure

  where
    testGroups =
      [ Test.Tutorial.testGroup
      , Test.OrgRoster.Tests.testGroup
      ]
