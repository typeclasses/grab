module BagParse.List.Types (Parser) where

import qualified BagParse.Parser.Types

type Parser item err a = BagParse.Parser.Types.Parser [item] err a
