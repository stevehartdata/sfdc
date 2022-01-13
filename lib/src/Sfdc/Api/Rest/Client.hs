{-# LANGUAGE OverloadedStrings #-}

module Sfdc.Api.Rest.Client
  ( QueryResponse
  , describeGlobal
  , sobjectDescribe
  , query
  , queryResults
  )
where

import Data.Text
import Servant.Auth
import Servant.Auth.Client
import Servant.API
import Servant.Client

import Sfdc.Api.Api
import Sfdc.Api.Client
import Sfdc.Api.OAuth2
import Sfdc.Api.Rest.Api
import Sfdc.Api.Rest.QueryResponse
import Sfdc.Api.SfdcClient (SfdcClient)
import qualified Sfdc.Api.SfdcClient as SC
import Sfdc.SObjectName
import Sfdc.SoqlQuery (SoqlQuery)


describeGlobal':: AccessToken -> ClientM DescribeGlobalResponse
describeGlobal' =
  (\(x :<|> _) -> x) . sObjectsApi . restApi . versionedApi . withAccessToken

describeGlobal :: SfdcClient DescribeGlobalResponse
describeGlobal = SC.withAccessToken $ \token -> describeGlobal' token

sobjectDescribe' :: AccessToken -> SObjectName -> ClientM SObjectDescribe
sobjectDescribe' =
  (\(_ :<|> x) -> x) . sObjectsApi . restApi . versionedApi . withAccessToken

sobjectDescribe :: SObjectName -> SfdcClient SObjectDescribe
sobjectDescribe obj_nm =
  SC.withAccessToken $ \token -> sobjectDescribe' token obj_nm

query' :: AccessToken -> SoqlQuery -> ClientM QueryResponse
query' =
  (\(x :<|> _) -> x) . restApi . versionedApi . withAccessToken

query :: SoqlQuery -> SfdcClient QueryResponse
query q = SC.withAccessToken $ \token -> query' token q

queryResults' :: AccessToken -> [Text] -> ClientM QueryResponse
queryResults' =
  (\(_ :<|> x) -> x) . withAccessToken

queryResults :: Text -> SfdcClient QueryResponse
queryResults locator =
  SC.withAccessToken $ \token -> queryResults' token (splitOn "/" locator)

restApi (_ :<|> x) = x
sObjectsApi (_ :<|> x) = x

