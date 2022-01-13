{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.ApiVersion
  ( ApiVersion
  )
where

import Data.Aeson
import GHC.Generics

-- FIXME: We can lose information here converting from decimal
newtype ApiVersion = ApiVersion Float
  deriving (Generic, Show, Eq, Ord)

instance FromJSON ApiVersion
