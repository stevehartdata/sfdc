module Sfdc.Api.Bulk2_0.Ingest.ClientSpec where

import Test.Hspec

import Data.Either (isRight, fromRight)
import Data.Either.Utils (forceEither)
import Servant.Client

import Config (getAuthConfig)
import Sfdc.Api.Bulk2_0.Ingest.TestData

import qualified Sfdc.Api.Bulk2_0.Ingest.Api as I
import Sfdc.Api.Bulk2_0.Ingest.Client
import Sfdc.Api.Bulk2_0.Ingest.CloseJobRequest as CloseJobRequest
import qualified Sfdc.Api.Bulk2_0.Ingest.CreateJobRequest as CreateJobRequest
import qualified Sfdc.Api.Bulk2_0.Ingest.IngestJobBatch as IJB
import qualified Sfdc.Api.Bulk2_0.Ingest.NewJobInfo as NJI
import Sfdc.Api.Session
import Sfdc.Api.SfdcClient
import Sfdc.SObjectName (account)

setup :: IO AuthConfig
setup = getAuthConfig

createIngestJob' :: AuthConfig -> IO I.NewJobInfo
createIngestJob' auth_config =
  runClient auth_config
    (createIngestJob
      (CreateJobRequest.CreateJobRequest account CreateJobRequest.Insert
        CreateJobRequest.CRLF))

testBatch :: IngestJobBatch
testBatch = IJB.ingestJobBatch testData

spec :: Spec
spec = do
  beforeAll setup $ do

    it "can create an ingest job" $ \auth_config -> do
      createIngestJob' auth_config
      return ()

    context "with ingest job" $ do
      beforeAllWith (\auth_config -> do
        job_info <- createIngestJob' auth_config
        return (auth_config, NJI.id $ job_info)) $ do
          it "can submit a batch" $ \(auth_config, job_id) -> do
            runClient auth_config $
                      submitIngestBatch job_id testBatch
            return ()

          it "can get the job info" $ \(auth_config, job_id) -> do
            runClient auth_config $
                    getJobInfo job_id
            return ()

          it "can close the job" $ \(auth_config, job_id) -> do
            runClient auth_config $
                      closeIngestJob job_id
                        (CloseJobRequest CloseJobRequest.Aborted)
            return ()

          it "can delete the job" $ \(auth_config, job_id) -> do
            runClient auth_config (deleteIngestJob job_id)
            return ()
