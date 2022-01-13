{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Bulk2_0.Ingest.JobInfo
  ( JobInfo (..)
  , JobState (..)
  )
where

import Data.Aeson
import Data.Text
import Data.Time.Clock
import GHC.Generics

import Sfdc.Api.ApiVersion

data JobInfo = JobInfo
  { apexProcessingTime :: Double
  , apiActiveProcessingTime :: Double
  , apiVersion :: ApiVersion
  , assignmentRuleId :: Maybe Text
  , columnDelimiter :: Text
  , concurrencyMode :: Text
  , contentType :: Text
  , contentUrl :: Maybe Text
  , createdById :: Text
  , createdDate :: UTCTime
  , externalIdFieldName :: Maybe Text
  , id :: Text
  , jobType :: Text
  , lineEnding :: Text
  , numberRecordsFailed :: Integer
  , numberRecordsProcessed :: Integer
  , object :: Text
  , operation :: Text
  , retries :: Integer
  , state :: JobState
  , systemModstamp :: UTCTime
  , totalProcessingTime :: Double
  } deriving (Show, Generic)

instance FromJSON JobInfo

data JobState
  = Open
  | UploadComplete
  | InProgress
  | Aborted
  | JobComplete
  | Failed
  deriving (Show, Generic, Eq, Ord)

instance FromJSON JobState
