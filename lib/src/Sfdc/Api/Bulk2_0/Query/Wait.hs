module Sfdc.Api.Bulk2_0.Query.Wait
  ( waitForJobToComplete
  )
where

import Control.Monad (void)
import Control.Monad.Catch
import Control.Retry

import Sfdc.SObjectName
import Sfdc.Api.Bulk2_0.Query.Client
import Sfdc.Api.Bulk2_0.Query.JobId
import qualified Sfdc.Api.Bulk2_0.Query.JobInfo as JobInfo
import Sfdc.Api.SfdcClient


data JobProcessingError = JobProcessingError JobId JobProcessingErrorCondition
  deriving (Show)

instance Exception JobProcessingError

data JobProcessingErrorCondition
  = JobAborted
  -- ^ The job was aborted.
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
        JobInfo.UploadComplete -> return JobNotComplete
        JobInfo.InProgress -> return JobNotComplete
        JobInfo.Aborted -> throwM $ JobProcessingError job_id JobAborted
        JobInfo.JobComplete -> return JobComplete
        JobInfo.Failed -> throwM $ JobProcessingError job_id JobFailed
