module Sfdc.Api.Bulk2_0.Insert
  ( insertBlocking
  )
where

import Sfdc.SObjectName
import Sfdc.Api.Bulk2_0.Ingest.Client
import qualified Sfdc.Api.Bulk2_0.Ingest.CloseJobRequest as CloseJobReq
import qualified Sfdc.Api.Bulk2_0.Ingest.CreateJobRequest as CreateJobReq
import Sfdc.Api.Bulk2_0.Ingest.Batches
import qualified Sfdc.Api.Bulk2_0.Ingest.NewJobInfo as NewJobInfo
import Sfdc.Api.Bulk2_0.Ingest.Wait
import Sfdc.Api.SfdcClient

-- SFDC does not actually support uploading multiple batches for one job, so we
-- instead have to create multiple jobs, each with one batch.
insertBlocking :: (DefaultOrdered record, ToNamedRecord record)
                => Session
                -> SObjectName
                -> [record]
                -> IO ()
insertBlocking sess target_obj xs = do
  let batches' = batches xs
  job_ids <- (flip mapM batches') $ \batch -> do
    runClientWithSession sess $ do
      new_job_info <- createIngestJob $ CreateJobReq.CreateJobRequest
                        { CreateJobReq.object = target_obj
                        , CreateJobReq.operation = CreateJobReq.Insert
                        , CreateJobReq.lineEnding = CreateJobReq.CRLF
                        }
      let job_id = NewJobInfo.id new_job_info
      submitIngestBatch job_id batch
      _ <- closeIngestJob job_id $ CloseJobReq.CloseJobRequest
                        { CloseJobReq.state = CloseJobReq.UploadComplete }
      return job_id
  mapM_ (waitForJobToComplete sess) job_ids
