{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Bulk2_0.Ingest.NewJobInfo
  ( NewJobInfo
  , Sfdc.Api.Bulk2_0.Ingest.NewJobInfo.id
  )
where

import Data.Aeson
import GHC.Generics

import Sfdc.Api.Bulk2_0.Ingest.JobId

data NewJobInfo = NewJobInfo
  { id :: JobId
  }
  deriving (Generic, Show)

instance FromJSON NewJobInfo
