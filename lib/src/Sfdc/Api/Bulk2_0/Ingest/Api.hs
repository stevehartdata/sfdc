{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Sfdc.Api.Bulk2_0.Ingest.Api
  ( IngestApi

  , CloseJobRequest
  , CloseJobResponse
  , CreateJobRequest
  , IngestJobBatch
  , JobId
  , JobInfo
  , NewJobInfo
  )
where

import Servant.API

import Sfdc.Api.Bulk2_0.Ingest.Csv
import Sfdc.Api.Bulk2_0.Ingest.CloseJobRequest
import Sfdc.Api.Bulk2_0.Ingest.CloseJobResponse
import Sfdc.Api.Bulk2_0.Ingest.CreateJobRequest
import Sfdc.Api.Bulk2_0.Ingest.IngestJobBatch
import Sfdc.Api.Bulk2_0.Ingest.JobId
import Sfdc.Api.Bulk2_0.Ingest.JobInfo
import Sfdc.Api.Bulk2_0.Ingest.NewJobInfo

type IngestApi =
  "ingest"
  :> (ReqBody '[JSON] CreateJobRequest :> Post '[JSON] NewJobInfo
      :<|> Capture "jobId" JobId
            :> ("batches" :> ReqBody '[Csv] IngestJobBatch :> PutCreated '[] NoContent
                :<|> ReqBody '[JSON] CloseJobRequest :> Patch '[JSON] CloseJobResponse
                :<|> DeleteNoContent '[] NoContent
                :<|> Get '[JSON] JobInfo))