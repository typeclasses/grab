module BagParse.Form.Errors
  ( Err_Missing (..)
  , Err_Duplicate (..)
  , Err_Unexpected (..)
  , Err_OnlyAllowed (..)
  ) where

import Data.Text (Text)

class Err_Missing a where
    -- | A parameter was expected, but none was given.
    err_missing :: a

class Err_Duplicate a where
    -- | A parameter was given repeatedly in a situation where it was expected to be present at most once.
    err_duplicate :: a

class Err_Unexpected a where
    -- | An unexpected parameter was given.
    err_unexpected :: a

class Err_OnlyAllowed a where
    -- | There is only one allowed value for a parameter, and something different was given.
    err_onlyAllowed :: Text {- ^ The allowed value -} -> a

instance Err_Missing () where err_missing = ()
instance Err_Duplicate () where err_duplicate = ()
instance Err_Unexpected () where err_unexpected = ()
instance Err_OnlyAllowed () where err_onlyAllowed = const ()
