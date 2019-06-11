import qualified Control.Grab as Grab

import Criterion.Main

import qualified Data.List as List
import Prelude hiding (filter)

parity :: Integer -> Maybe (Integer, Integer)
parity n = Grab.desideratum . Grab.runGrab [1..n] $
  (,) <$> (sum <$> filter even) <*> (sum <$> filter odd)

filter :: (a -> Bool) -> Grab.Simple [a] () [a]
filter p = Grab.partition (List.partition p)

main :: IO ()
main = defaultMain [
  bgroup "parity"
    (
      (\i -> let n = i * 10000 in
        bench (show i) (nf parity n)
      )
      <$> [1..3]
    )
  ]
