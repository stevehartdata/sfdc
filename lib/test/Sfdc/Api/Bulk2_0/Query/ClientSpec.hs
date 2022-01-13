{-# LANGUAGE OverloadedStrings #-}

module Sfdc.Api.Bulk2_0.Query.ClientSpec where

import Test.Hspec

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isRight, fromRight)
import Data.Text (Text)
import Numeric.Natural
import Servant.Client

import Config

import Sfdc.Api.Bulk2_0.Query.Client
import Sfdc.Api.Session
import Sfdc.Api.SfdcClient
import qualified Sfdc.Api.Bulk2_0.Query.QueryJobResponse as QJR
import qualified Sfdc.Api.Bulk2_0.Query.JobInfo as QJS
import Sfdc.SoqlQuery

setup :: IO AuthConfig
setup = getAuthConfig

submitQueryJob' :: AuthConfig -> IO QueryJobResponse
submitQueryJob' auth_config =
  runClient auth_config
    $ submitQueryJob (queryRequest $ soqlQueryFromText "SELECT Id FROM Account")

waitForQueryJobToComplete :: AuthConfig -> JobId -> IO ()
waitForQueryJobToComplete auth_config job_id = do
  sess <- newSession auth_config
  wait sess 10
  where
    wait :: Session -> Natural -> IO ()
    wait _ 0 = error "Query job did not complete in time"
    wait sess attempts = do
      s <- runClientWithSession sess $ getJobInfo job_id
      case QJS.state s of
        QJS.UploadComplete -> continue
        QJS.InProgress -> continue
        QJS.Aborted -> error "Query job aborted"
        QJS.JobComplete -> return ()
        QJS.Failed -> error "Query job failed"
      where
        continue = do
          liftIO $ threadDelay 1000000 -- wait 1 second
          wait sess (attempts - 1)

spec :: Spec
spec = do
  beforeAll setup $ do

    it "can submit a query job" $ \auth_config -> do
      submitQueryJob' auth_config
      return ()

    context "with submitted query job" $ do
      beforeAllWith
        (\auth_config -> do
          qjr <- submitQueryJob' auth_config
          let job_id = QJR.id $ qjr
          return (auth_config, job_id))
        $ do
          it "can check the status of a query job" $ \(auth_config, job_id) -> do
            runClient auth_config $ getJobInfo job_id
            return ()

          context "with completed query job" $ do
            beforeAllWith
              (\x@(auth_config, job_id) -> do
                waitForQueryJobToComplete auth_config job_id
                return x)
              $ do
                it "can retrieve query job results" $ \(auth_config, job_id) -> do
                  runClient auth_config $
                    queryJobResults job_id Nothing Nothing
                  return ()
