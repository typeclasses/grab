module BagParse.Prelude where

import BagParse.Core

trivialP_maybe :: Maybe a -> Parser bag item () a
trivialP_maybe = \case Just x -> parseSuccess x; Nothing -> parseFailure ()

trivialP_either :: Either err a -> Parser bag item err a
trivialP_either = \case Left x -> parseFailure x; Right x -> parseSuccess x

matchAny :: Parser bag item err ()
matchAny = _

parseSuccess :: a -> Parser bag item err a
parseSuccess = Parser_Success

parseFailure :: err -> Parser bag item err a
parseFailure = Parser_Failure

one :: e -> Parser [] t e [a] -> Parser [] t e a
one p = _
