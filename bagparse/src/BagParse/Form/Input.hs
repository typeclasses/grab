module BagParse.Form.Input
  ( Form (..), Param (..)
  ) where

import BagParse.Form.Name

import Data.Text (Text)

data Form =
  Form
    { formParams :: [Param]
    , formContext :: Name -> Name
    }

data Param =
  Param
    { paramName  :: Name
    , paramValue :: Text
    }

deriving stock instance Eq Param
deriving stock instance Ord Param
deriving stock instance Show Param
