{-# LANGUAGE TypeOperators, DataKinds, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sfdc.Api.Api
  ( api
  )
where

import Servant.API
import Servant.Auth as SA
import Data.Proxy

import Sfdc.Api.Bulk2_0.Api
import Sfdc.Api.Rest.Api


type Api
  = Auth '[SA.JWT] ()
    :> ("services" :> "data" :> Capture "version" String
          :> (BulkApi2_0 :<|> RestApi)
        :<|> QueryResultsApi)

api :: Proxy Api
api = Proxy
