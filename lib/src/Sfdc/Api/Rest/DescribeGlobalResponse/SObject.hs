{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Rest.DescribeGlobalResponse.SObject
  ( SObject (..)
  )
where

import Data.Aeson
import Data.Text
import GHC.Generics

import Sfdc.SObjectName (SObjectName)

data SObject = SObject
  { custom :: Bool
  , name :: SObjectName
  , keyPrefix :: Maybe Text
  , queryable :: Bool
  } deriving (Generic, Show)

instance FromJSON SObject
