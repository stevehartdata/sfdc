{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Rest.SObjectDescribe.FieldInfo
  ( FieldInfo (..)
  )
where

import Data.Aeson
import Data.Text
import GHC.Generics

import Sfdc.SObjectFieldName

data FieldInfo = FieldInfo
  { length :: Integer
  , name :: SObjectFieldName
  , _type :: Text
  } deriving (Generic, Show)

instance FromJSON FieldInfo where
  parseJSON = genericParseJSON $
    defaultOptions
      { fieldLabelModifier = stripLeadingUnderscore }
    where
      stripLeadingUnderscore ('_':xs) = xs
      stripLeadingUnderscore xs = xs