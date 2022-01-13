{-# LANGUAGE RankNTypes #-}

module Sfdc.Api.ClientRunner
  ( Runner
  , getRunner
  , runClient
  )
where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client

import qualified Config as C
import Sfdc.Api.OAuth2Spec (getAccessTokenWithConfig) -- FIXME: Import from somewhere else

import Sfdc.Api.OAuth2

newtype Runner = Runner (forall a. (AccessToken -> ClientM a) -> IO (Either ClientError a))

runClient :: Runner -> (AccessToken -> ClientM a) -> IO (Either ClientError a)
runClient (Runner r) f = r f

getRunner :: IO Runner
getRunner = do
  config <- C.getConfig
  token_response <- getAccessTokenWithConfig config 600
  let access_token = accessToken token_response
  manager' <- newTlsManager
  url <- parseBaseUrl . instanceUrl $ token_response
  let runClient' =
          flip runClientM
            (mkClientEnv manager' url)
  return $ Runner $ \f -> runClient' $ f access_token