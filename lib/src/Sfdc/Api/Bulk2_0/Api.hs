{-# LANGUAGE TypeOperators, DataKinds, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sfdc.Api.Bulk2_0.Api
  ( BulkApi2_0
  , QueryRequest
  , QueryJobResponse
  )
where

import Servant.API
import Servant.Auth as SA
import Data.Proxy

import Sfdc.Api.OAuth2 (AccessToken (..))
import Sfdc.Api.Bulk2_0.Ingest.Api
import Sfdc.Api.Bulk2_0.Query.Api


type BulkApi2_0
  = "jobs" :> (QueryApi :<|> IngestApi)





