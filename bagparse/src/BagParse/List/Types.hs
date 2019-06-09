module BagParse.List.Types
  (

    Parser
  , Result
  , BagParse.Parser.Types.Yield

  ) where

import qualified BagParse.Parser.Types

type Parser item log a =
    BagParse.Parser.Types.Parser [item] log a

type Result item log a =
    BagParse.Parser.Types.Result [item] log a
