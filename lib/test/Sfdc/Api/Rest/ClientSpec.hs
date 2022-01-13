{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sfdc.Api.Rest.ClientSpec where

import Test.Hspec

import Control.Monad (void)
import Data.Either
import Data.Maybe
import Data.Text (Text)
import Servant.API
import Servant.Client

import Config

import Sfdc.Api.Bulk2_0.Delete (cascadeDeleteAllBlocking)
import Sfdc.Api.Bulk2_0.Ingest.TestData (infiniteTestData)
import Sfdc.Api.Bulk2_0.Insert (insertBlocking)
import Sfdc.Api.Rest.Client
import qualified Sfdc.Api.Rest.QueryResponse as QR
import Sfdc.Api.Session
import Sfdc.Api.SfdcClient
import Sfdc.SObjectName (sObjectName, account)
import Sfdc.SoqlQuery

setup :: IO AuthConfig
setup = getAuthConfig

query' :: SfdcClient QueryResponse
query' = query $ soqlQueryFromText "SELECT Id FROM Account"

spec :: Spec
spec = do
  beforeAll setup $ do
    it "can describe global" $ \auth_config -> do
      runClient auth_config describeGlobal
      return ()

    it "can describe an object" $ \auth_config -> do
      runClient auth_config
              (sobjectDescribe account)
      return ()

    it "can submit a query" $ \auth_config -> do
      runClient auth_config query'
      return ()

    describe "query with results in more than one response" $ do
      beforeAllWith
        (\auth_config -> do
          sess <- newSession auth_config
          populateLargeTestData sess
          next_records <- largeQuery sess
          return (sess, next_records)) $ do
          -- The REST API returns at most 2000 records in a response, so we need
          -- a query that returns more than 2000 results.

          it "can retrieve the second set of results" $ \(sess, next_records) -> do
            runClientWithSession sess (queryResults next_records)
            return ()

populateLargeTestData :: Session -> IO ()
populateLargeTestData sess = do
  cascadeDeleteAllBlocking sess account
  insertBlocking sess account (take 2001 infiniteTestData)

largeQuery :: Session -> IO Text
largeQuery sess = do
  resp <- runClientWithSession sess query'
  return
    . fromMaybe (error "Expected query to have more than one result batch")
    . QR.nextRecordsUrl $ resp