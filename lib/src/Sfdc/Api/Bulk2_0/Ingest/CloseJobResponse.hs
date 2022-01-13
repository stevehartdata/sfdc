{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Bulk2_0.Ingest.CloseJobResponse
  (CloseJobResponse)
where

import Data.Aeson
import GHC.Generics

import Sfdc.Api.Bulk2_0.Ingest.JobId

data CloseJobResponse = CloseJobResponse
  { id :: JobId
  , state :: JobState
  }
  deriving (Generic, Show)

instance FromJSON CloseJobResponse

data JobState
  = Open
  | UploadComplete
  | Aborted
  | JobComplete
  | Failed
  deriving (Generic, Show)

instance FromJSON JobState
