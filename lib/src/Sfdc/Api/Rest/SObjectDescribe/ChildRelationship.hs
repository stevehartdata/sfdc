{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Rest.SObjectDescribe.ChildRelationship
  ( ChildRelationship (..)
  )
where

import Data.Aeson
import Data.Text
import GHC.Generics

import Sfdc.SObjectFieldName (SObjectFieldName)
import Sfdc.SObjectName (SObjectName)

data ChildRelationship = ChildRelationship
  { field :: SObjectFieldName
  , childSObject :: SObjectName
  , deprecatedAndHidden :: Bool
  , cascadeDelete :: Bool
  , relationshipName :: Maybe Text
  , restrictedDelete :: Maybe Bool
  } deriving (Generic, Show)

instance FromJSON ChildRelationship
