{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Sfdc.Api.Bulk2_0.Query.Api
  ( QueryApi

  , JobInfo
  , QueryRequest
  , QueryJobResponse
  )
where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text as T
import Numeric.Natural
import Servant.API

import Sfdc.Api.Bulk2_0.Query.JobId (JobId)
import Sfdc.Api.Bulk2_0.Query.QueryJobResponse
import Sfdc.Api.Bulk2_0.Query.JobInfo
import Sfdc.Api.Bulk2_0.Query.QueryRequest

type QueryApi =
  "query"
  :> ( ReqBody '[JSON] QueryRequest :> Post '[JSON] QueryJobResponse
       :<|> Capture "jobId" JobId
            :> ( Get '[JSON] JobInfo
                 :<|> "results"
                      :> QueryParam "locator" Text
                      :> QueryParam "maxRecords" Natural
                      :> Get '[OctetStream]
                             (Headers '[ Header "Sforce-NumberOfRecords" Natural
                                       , Header "Sforce-Locator" Text
                                       ]
                                      ByteString)))

