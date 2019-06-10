module BagParse.Form.Types
  (
  -- * The main type
    X.Action (..)

  -- * Type aliases
  , Grab, Dump, Result, Product

  ) where

import qualified BagParse.Parser.Types as X

import BagParse.Form.Input
import BagParse.Form.Log
import BagParse.Form.Name

import Data.Coerce
import Data.String

import Numeric.Natural (Natural)

type Grab err a =
    X.Grab Form (Log err) a

type Result err a =
    X.Result Form (Log err) a

type Dump err a =
    X.Dump Form (Log err) a

type Product err a =
    X.Product (Log err) a
