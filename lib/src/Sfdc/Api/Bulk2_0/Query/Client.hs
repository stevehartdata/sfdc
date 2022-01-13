{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Sfdc.Api.Bulk2_0.Query.Client
  ( JobId
  , QueryRequest
  , QueryJobResponse

  , submitQueryJob
  , getJobInfo
  , queryJobResults
  , queryRequest
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Text
import Numeric.Natural
import Servant.API
import Servant.Client

import Sfdc.Api.Bulk2_0.Api
import Sfdc.Api.Bulk2_0.Query.JobId (JobId)
import Sfdc.Api.Bulk2_0.Query.JobInfo (JobInfo)
import Sfdc.Api.Bulk2_0.Query.QueryRequest (queryRequest)
import Sfdc.Api.Client
import Sfdc.Api.SfdcClient (SfdcClient)
import qualified Sfdc.Api.SfdcClient as SC
import Sfdc.Api.OAuth2 (AccessToken)


-- | Submit a query job
submitQueryJob' :: AccessToken -> QueryRequest -> ClientM QueryJobResponse
submitQueryJob' =
  (\(x :<|> _) -> x) . queryApi . bulkApi . versionedApi . withAccessToken

submitQueryJob :: QueryRequest -> SfdcClient QueryJobResponse
submitQueryJob qr =
  SC.withAccessToken $ \token -> submitQueryJob' token qr

-- | Get information about a query job
getJobInfo' :: AccessToken
               -> JobId -- ^ Query job ID
               -> ClientM JobInfo
getJobInfo' =
  withQueryJob (\(x :<|> _) -> x) . queryApi . bulkApi . versionedApi
  . withAccessToken

getJobInfo :: JobId -> SfdcClient JobInfo
getJobInfo job_id =
  SC.withAccessToken $ \token -> getJobInfo' token job_id

queryJobResults' :: AccessToken
                 -> JobId -- ^ Query job ID
                 -> Maybe Text -- ^ Locator
                 -> Maybe Natural -- ^ Max records to return
                 -> ClientM
                      (Headers
                        '[ Header "Sforce-NumberOfRecords" Natural
                         , Header "Sforce-Locator" Text
                         ]
                        ByteString)
queryJobResults' =
  withQueryJob (\(_ :<|> x) -> x) . queryApi . bulkApi . versionedApi
  . withAccessToken

queryJobResults job_id locator max_records =
  SC.withAccessToken $ \token ->
    queryJobResults' token job_id locator max_records

withQueryJob f = \(_ :<|> x) -> f . x

queryApi (x :<|> _) = x