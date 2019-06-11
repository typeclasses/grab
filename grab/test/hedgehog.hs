{-# LANGUAGE

    BlockArguments, LambdaCase, OverloadedStrings,
    ScopedTypeVariables, TemplateHaskell, ViewPatterns

#-}

import qualified Control.Grab as G

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
        r = G.log "a" *> G.log "b" *> G.log "c" :: G.Product String Integer
    G.toLog r === "abc"
    G.toValueMaybe r === Nothing

prop_2 :: Property
prop_2 = withTests 1 $ property
  do
    let
        r = G.log "a" *> G.log "b" *> G.value 4
    G.toLog r === "ab"
    G.toValueMaybe r === Nothing

prop_3 :: Property
prop_3 = withTests 1 $ property
  do
    let
        r = G.value 4 :: G.Product String Integer
    G.toValueMaybe r === Just 4
