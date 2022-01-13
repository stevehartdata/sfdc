{-# LANGUAGE TypeOperators, DataKinds, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sfdc.Api.Rest.Api
  ( QueryResultsApi
  , RestApi
  , DescribeGlobalResponse
  , SObjectDescribe
  )
where

import Servant.API
import Servant.Auth as SA
import Data.Text (Text)

import Sfdc.Api.Rest.DescribeGlobalResponse
import Sfdc.Api.Rest.SObjectDescribe
import Sfdc.Api.Rest.QueryResponse
import Sfdc.SObjectName (SObjectName)
import Sfdc.SoqlQuery (SoqlQuery)

type RestApi
  = ("query"
     :> QueryParam' '[Required, Strict] "q" SoqlQuery :> Get '[JSON] QueryResponse)
    :<|> ("sobjects"
          :> (Get '[JSON] DescribeGlobalResponse
              :<|> Capture "object name" SObjectName :> "describe" :> Get '[JSON] SObjectDescribe))

type QueryResultsApi = CaptureAll "segments" Text :> Get '[JSON] QueryResponse
