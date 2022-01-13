{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Bulk2_0.Ingest.CreateJobRequest
  ( CreateJobRequest (..)
  , LineEnding (..)
  , Operation (..)
  , SObjectName
  )
where

import Data.Aeson
import Data.Char (toLower)
import GHC.Generics

import Sfdc.SObjectName

data CreateJobRequest = CreateJobRequest
  { object :: SObjectName
  , operation :: Operation
  , lineEnding :: LineEnding
  }
  deriving (Generic)

instance ToJSON CreateJobRequest

data Operation
  = Insert
  | Delete
  | HardDelete
  | Update
  | Upsert
  deriving (Generic)

instance ToJSON Operation where
  toJSON = genericToJSON defaultOptions { constructorTagModifier = map toLower }

data LineEnding
  = LF
  | CRLF
  deriving (Generic, Eq)

instance ToJSON LineEnding
