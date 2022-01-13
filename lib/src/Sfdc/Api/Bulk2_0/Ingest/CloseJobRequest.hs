{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Bulk2_0.Ingest.CloseJobRequest
  ( CloseJobRequest (..)
  , JobState (..)
  )
where

import Data.Aeson
import GHC.Generics

data CloseJobRequest = CloseJobRequest
  { state :: JobState
  }
  deriving (Generic)

instance ToJSON CloseJobRequest

data JobState
  = UploadComplete
  | Aborted
  deriving (Generic)

instance ToJSON JobState
