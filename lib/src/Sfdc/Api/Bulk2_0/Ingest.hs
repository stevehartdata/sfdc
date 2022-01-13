{-# LANGUAGE OverloadedStrings #-}

module Sfdc.Api.Bulk2_0.Ingest
  ( module Sfdc.Api.Bulk2_0.Ingest.Client
  , abortJob
  , InvalidJobStateException (..)
  )
where

import Control.Monad.Catch
import Data.Aeson (eitherDecode)
import Network.HTTP.Types.Status (status400)
import Servant.Client.Core (responseBody, responseStatusCode)
import Servant.Client.Core.ClientError (ClientError (FailureResponse))

import qualified Sfdc.Api.Bulk2_0.Ingest.ApiError as ApiError
import Sfdc.Api.Bulk2_0.Ingest.Client
import qualified Sfdc.Api.Bulk2_0.Ingest.CloseJobRequest as CloseJobReq

data InvalidJobStateException = AttemptedToAbortClosedJob
  deriving (Show)
instance Exception InvalidJobStateException

abortJob :: JobId -> SfdcClient CloseJobResponse
abortJob job_id =
  (closeIngestJob job_id $ CloseJobReq.CloseJobRequest
            { CloseJobReq.state = CloseJobReq.Aborted })
  `catch` \e ->
    case e of
      FailureResponse _ resp ->
        let error_e = eitherDecode . responseBody $ resp :: Either String [ApiError.ApiError]
        in flip (either (\e -> error $ "Couldn't decode" ++ e)) error_e $ \errors' ->
            if any (\error' ->
                responseStatusCode resp == status400
                && ApiError.errorCode error' == "INVALIDJOBSTATE"
                && ApiError.message error' == "Aborting already Completed Job not allowed") errors'
              then throwM AttemptedToAbortClosedJob
              else throwM e
      _ -> throwM e