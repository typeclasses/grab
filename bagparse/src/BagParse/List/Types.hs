module BagParse.List.Types
  (
  -- * The main type
    X.Action (..)

  -- * Type aliases
  , Grab, Dump, Result, Product

  ) where

import qualified BagParse.Parser.Types as X

type Grab item log value =
    X.Grab [item] log value

type Result item log value =
    X.Result [item] log value

type Dump item log value =
    X.Dump [item] log value

type Product log value =
    X.Product log value
