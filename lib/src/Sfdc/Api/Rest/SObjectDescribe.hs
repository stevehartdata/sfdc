{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Rest.SObjectDescribe
  ( SObjectDescribe (..)
  )
where

import Data.Aeson
import Data.List.NonEmpty
import Data.Text
import GHC.Generics

import Sfdc.Api.Rest.SObjectDescribe.ChildRelationship
import Sfdc.Api.Rest.SObjectDescribe.FieldInfo

data SObjectDescribe = SObjectDescribe
  { name :: Text
  , fields :: NonEmpty FieldInfo
  , keyPrefix :: Maybe Text
  , queryable :: Bool
  , childRelationships :: [ChildRelationship]
  } deriving (Generic, Show)

instance FromJSON SObjectDescribe
