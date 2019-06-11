import qualified BagParse.Parser.Prelude as X
import BagParse.Parser.Types

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
        r = X.log "a" *> X.log "b" *> X.log "c" :: Product String Integer
    X.toLog r === "abc"
    X.toValueMaybe r === Nothing

prop_2 :: Property
prop_2 = withTests 1 $ property
  do
    let
        r = X.log "a" *> X.log "b" *> X.value 4
    X.toLog r === "ab"
    X.toValueMaybe r === Nothing

prop_3 :: Property
prop_3 = withTests 1 $ property
  do
    let
        r = X.value 4 :: Product String Integer
    X.toValueMaybe r === Just 4
