import qualified Test.OrgRoster.Tests

import Control.Monad (when)

import Numeric.Natural

import System.IO (hSetEncoding, stdout, stderr, utf8)
import System.Exit (exitFailure)

import Hedgehog

main :: IO ()
main =
  do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    ok <- checkParallel Test.OrgRoster.Tests.group
    when (not ok) exitFailure
