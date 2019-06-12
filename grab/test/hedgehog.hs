{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

{-# LANGUAGE

    BlockArguments, LambdaCase, OverloadedStrings,
    ScopedTypeVariables, TemplateHaskell, ViewPatterns

#-}

import qualified Control.Grab as Grab

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (when)

import System.IO (hSetEncoding, stdout, stderr, utf8)
import System.Exit (exitFailure)

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
prop_1 = withTests 1 $ property
  do
    let
        r = Grab.failure "a" *>
            Grab.failure "b" *>
            Grab.failure "c"
            :: Grab.Extract String Integer

    Grab.log r === "abc"
    Grab.desideratum r === Nothing

prop_2 :: Property
prop_2 = withTests 1 $ property
  do
    let
        r = Grab.failure "a" *>
            Grab.failure "b" *>
            Grab.success (4 :: Integer)

    Grab.log r === ("ab" :: String)
    Grab.desideratum r === Nothing

prop_3 :: Property
prop_3 = withTests 1 $ property
  do
    let
        r = Grab.success 4 :: Grab.Extract String Integer

    Grab.desideratum r === Just 4
