{-# OPTIONS_GHC

    -Wall
    -fno-warn-unused-imports
    -fno-warn-missing-signatures

#-}

{-# LANGUAGE

    BlockArguments, LambdaCase, OverloadedStrings,
    ScopedTypeVariables, TemplateHaskell, ViewPatterns

#-}

import qualified Control.Grab as Grab
import Control.Grab ((/))

import Prelude hiding ((/))

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (when)
import qualified Data.List as List

import System.IO (hSetEncoding, stdout, stderr, utf8)
import System.Exit (exitFailure)

tests :: IO Bool
tests =
  checkParallel $$(discover)

example =
    withTests 1 . property

x ~> y =
    example (x === y)

main :: IO ()
main =
  do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    ok <- tests
    when (not ok) exitFailure

prop_1 =
    let
        r = Grab.failure "a" *>
            Grab.failure "b" *>
            Grab.failure "c"
            :: Grab.Extract String Integer
    in
        (Grab.log r, Grab.desideratum r)
        ~>
        ("abc", Nothing)

prop_2 =
    let
        r = Grab.failure "a" *>
            Grab.failure "b" *>
            Grab.success (4 :: Integer)
    in
        (Grab.log r, Grab.desideratum r )
        ~>
        ("ab" :: String, Nothing)

prop_3 =
    let
        r = Grab.success 4 :: Grab.Extract String Integer
    in
        Grab.desideratum r ~> Just 4

prop_4 =
    let
        g :: Grab.Simple [Integer] () (Integer, Integer)
        g =
            (,)
                <$> ( Grab.partition (List.partition even)
                    / Grab.dump (\_ -> Grab.failure ())
                    )
                <*> ( Grab.partition (List.partition odd)
                    / Grab.dump (\xs -> Grab.success (sum xs))
                    )

        r = Grab.runGrab g undefined

    in
        Grab.desideratum r ~> Nothing

prop_5 =
    let
        g :: Grab.Simple [Integer] () (Integer, Integer)
        g =
            (,)
                <$> ( Grab.partition (List.partition even)
                    / Grab.dump (\xs -> Grab.success (sum xs))
                    )
                <*> ( Grab.partition (List.partition odd)
                    / Grab.dump (\_ -> Grab.failure ())
                    )

        r = Grab.runGrab g undefined

    in
        Grab.desideratum r ~> Nothing

prop_6 =
    let
        g :: Grab.Simple [Integer] () Integer
        g =
            ( Grab.partition (List.partition even)
            / Grab.dump (\xs -> Grab.success (sum xs))
            )

        r = Grab.runGrab g [1,2,3,4]

    in
        Grab.desideratum r ~> Just 6

prop_7 =
    let
        g :: Grab.Simple [Integer] () (Integer, Integer)
        g =
            (,)
                <$> ( Grab.partition (List.partition even)
                    / Grab.dump (\xs -> Grab.success (sum xs))
                    )
                <*> ( Grab.partition (List.partition odd)
                    / Grab.dump (\xs -> Grab.success (sum xs))
                    )

        r = Grab.runGrab g [1,2,3,4]

    in
        Grab.desideratum r ~> Just (6, 4)

prop_8 =
    let
        g :: Grab.Simple [Integer] () (Integer, Integer)
        g =
            (,)
                <$> ( Grab.partition (List.partition even)
                    / Grab.dump (\xs -> Grab.success (sum xs))
                    )
                <*> ( Grab.partition (List.partition odd)
                    / Grab.dump (\xs -> if elem 3 xs then Grab.failure () else Grab.success (sum xs))
                    )

        r = Grab.runGrab g (1:2:3:undefined)

    in
        Grab.desideratum r ~> Nothing
