{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ApplicativeDo, BlockArguments #-}

import Prelude hiding ((/))

import qualified Control.Grab as Grab
import Control.Grab ((/))

import qualified Data.List as List
import qualified Data.Foldable as Foldable

import Data.Monoid

main :: IO ()
main =
  do
    let r = Grab.runGrab g [1..30000]
    putStrLn ("desideratum: " ++ show (Grab.desideratum r))
    putStrLn ("log: " ++ show (Grab.log r))

f :: Monoid log => (a -> Bool) -> Grab.Simple [a] log [a]
f p = Grab.partition (List.partition p)

g :: Grab.Simple [Integer] (Sum Integer) (Integer, Integer)
g =
  do
    _ <- Foldable.for_ primes \n ->
           ( f (\x -> mod x n == 0)
           / Grab.dump (\xs -> Grab.warning (Sum $! sum xs))
           )

    evenSum  <- sum <$> f even
    oddSum   <- sum <$> f odd

    pure (evenSum, oddSum)

primes :: [Integer]
primes = [5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79]
