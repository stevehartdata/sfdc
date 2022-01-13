{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sfdc.Api.FieldTypes.Id
  ( Id
  )
where

import Data.Aeson
import Data.Csv
import Data.Text (Text)
import GHC.Generics

newtype Id = Id Text
  deriving (FromJSON, FromField, ToField)
