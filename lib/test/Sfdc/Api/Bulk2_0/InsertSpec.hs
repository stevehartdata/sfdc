module Sfdc.Api.Bulk2_0.InsertSpec
  ( spec )
where

import Test.Hspec

import Control.Monad.Except
import Data.Either (isRight)

import Config

import Sfdc.Api.Bulk2_0.Ingest.TestData
import Sfdc.Api.Session
import Sfdc.Api.Bulk2_0.Insert
import Sfdc.SObjectName (account)


setup :: IO Session
setup = getAuthConfig >>= newSession

spec :: Spec
spec = do
  beforeAll setup $ do
    it "can insert one record" $ \sess -> do
      insertBlocking sess account (take 1 testData)

    it "can insert records that require multiple batches" $ \sess -> do
      -- This job should require two batches, since the data should be larger
      -- than 100 MB.
      insertBlocking sess account (take 6000000 infiniteTestData)
