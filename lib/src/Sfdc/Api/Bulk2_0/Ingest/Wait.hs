module Sfdc.Api.Bulk2_0.Ingest.Wait
  ( waitForJobToComplete
  )
where

import Control.Monad (void)
import Control.Monad.Catch
import Control.Retry

import Sfdc.SObjectName
import Sfdc.Api.Bulk2_0.Ingest.Client
import qualified Sfdc.Api.Bulk2_0.Ingest.JobInfo as JobInfo
import Sfdc.Api.SfdcClient


data JobProcessingError = JobProcessingError JobId JobProcessingErrorCondition
  deriving (Show)

instance Exception JobProcessingError

data JobProcessingErrorCondition
  = JobUnexpectedlyOpen
  -- ^ The job was unexpectedly open at a time when it should not have been.
  | JobAborted
  -- ^ The job was aborted.
  | JobRecordsFailed Integer
  -- ^ Some records failed to be processed.
  | JobFailed
  -- ^ The job failed.
  deriving (Show)

data JobCompletionStatus
  = JobComplete
  | JobNotComplete
  deriving (Eq)

waitForJobToComplete :: Session -> JobId -> IO ()
waitForJobToComplete sess job_id =
  void $ retrying (exponentialBackoff 1000000) (const $ return . (/= JobComplete)) $ \_ -> do
      job_info <- runClientWithSession sess $ getJobInfo job_id
      case JobInfo.state job_info of
        JobInfo.Open -> throwM $ JobProcessingError job_id JobUnexpectedlyOpen
        JobInfo.UploadComplete -> return JobNotComplete
        JobInfo.InProgress -> return JobNotComplete
        JobInfo.Aborted -> throwM $ JobProcessingError job_id JobAborted
        JobInfo.JobComplete ->
          if JobInfo.numberRecordsFailed job_info /= 0
            then throwM $ JobProcessingError job_id $ JobRecordsFailed (JobInfo.numberRecordsFailed job_info)
            else return JobComplete
        JobInfo.Failed -> throwM $ JobProcessingError job_id JobFailed
