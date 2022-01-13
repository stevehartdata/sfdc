{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Sfdc.Api.Bulk2_0.Ingest.Client
  ( AccessToken
  , CloseJobRequest
  , CloseJobResponse
  , CreateJobRequest
  , IngestJobBatch
  , JobId
  , NewJobInfo
  , SfdcClient

  , createIngestJob
  , submitIngestBatch
  , closeIngestJob
  , deleteIngestJob
  , getJobInfo
  )
where

import Servant.API
import Servant.Client

import Sfdc.Api.Bulk2_0.Ingest.Api
import Sfdc.Api.Client
import Sfdc.Api.OAuth2 (AccessToken)
import Sfdc.Api.SfdcClient (SfdcClient)
import qualified Sfdc.Api.SfdcClient as SC

createIngestJob' :: AccessToken
                -> CreateJobRequest
                -> ClientM NewJobInfo
createIngestJob' =
  (\(x :<|> _) -> x) . ingestApi . bulkApi . versionedApi . withAccessToken

createIngestJob :: CreateJobRequest
                  -> SfdcClient NewJobInfo
createIngestJob cjr = SC.withAccessToken$ \token -> createIngestJob' token cjr

submitIngestBatch' :: AccessToken
                  -> JobId
                  -> IngestJobBatch
                  -> ClientM NoContent
submitIngestBatch' =
  withIngestJob (\(x :<|> _) -> x) . ingestApi . bulkApi . versionedApi
  . withAccessToken

submitIngestBatch :: JobId -> IngestJobBatch -> SfdcClient NoContent
submitIngestBatch job_id batch =
  SC.withAccessToken $ \token -> submitIngestBatch' token job_id batch

closeIngestJob' :: AccessToken
                -> JobId
                -> CloseJobRequest
                -> ClientM CloseJobResponse
closeIngestJob' =
  withIngestJob (\(_ :<|> x :<|> _) -> x) . ingestApi . bulkApi . versionedApi
  . withAccessToken

closeIngestJob :: JobId -> CloseJobRequest -> SfdcClient CloseJobResponse
closeIngestJob job_id req =
  SC.withAccessToken $ \token -> closeIngestJob' token job_id req

deleteIngestJob' :: AccessToken
                -> JobId
                -> ClientM NoContent
deleteIngestJob' =
  withIngestJob (\(_ :<|> _ :<|> x :<|> _) -> x) . ingestApi . bulkApi
  . versionedApi . withAccessToken

deleteIngestJob :: JobId -> SfdcClient NoContent
deleteIngestJob job_id =
  SC.withAccessToken $ \token -> deleteIngestJob' token job_id

getJobInfo' :: AccessToken
            -> JobId
            -> ClientM JobInfo
getJobInfo' =
  withIngestJob (\(_ :<|> _ :<|> _ :<|> x) -> x) . ingestApi . bulkApi
  . versionedApi . withAccessToken

getJobInfo :: JobId -> SfdcClient JobInfo
getJobInfo job_id =
  SC.withAccessToken $ \token -> getJobInfo' token job_id

withIngestJob f = \(_ :<|> x) -> f . x

ingestApi (_ :<|> x) = x