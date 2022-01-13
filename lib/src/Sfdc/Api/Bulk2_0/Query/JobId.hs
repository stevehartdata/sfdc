{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Bulk2_0.Query.JobId
  (JobId)
where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant.API

-- Bulk API 2.0 Query Job IDs are used in a context distinct from Ingest Job
-- IDs, so we have distinct types.
data JobId
  = JobId Text
  deriving (Generic, Show)

instance FromJSON JobId

instance ToHttpApiData JobId where
  toUrlPiece (JobId st) = toUrlPiece st
