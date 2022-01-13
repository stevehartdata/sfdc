{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sfdc.Api.SfdcClient
  ( AuthConfig
  , Session
  , SfdcClient
  , withAccessToken
  , runClient
  , runClientWithSession
  )
where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Network.HTTP.Types (status401)
import Servant.Client (ClientM, ClientError (..), responseStatusCode
  , runClientM)
import qualified Servant.Client as Servant

import Sfdc.Api.Session hiding (accessToken, clientEnv, reportInvalidToken)
import qualified Sfdc.Api.Session as Session

newtype SfdcClient a = SfdcClient (ReaderT Session IO a)
  deriving (Applicative, Functor, Monad, MonadThrow, MonadCatch)

runClient :: AuthConfig -> SfdcClient a -> IO a
runClient auth_config c = do
  sess <- newSession auth_config
  runClientWithSession sess c

runClientWithSession :: Session -> SfdcClient a -> IO a
runClientWithSession sess (SfdcClient m) = runReaderT m sess

accessToken :: SfdcClient AccessToken
accessToken = do
  sess <- session
  SfdcClient $ liftIO $ Session.accessToken sess

clientEnv :: SfdcClient ClientEnv
clientEnv = do
  sess <- session
  SfdcClient $ return $ Session.clientEnv sess

session :: SfdcClient Session
session = SfdcClient $ ask

reportInvalidToken :: AccessToken -> SfdcClient ()
reportInvalidToken token = do
  sess <- session
  SfdcClient $ liftIO $ Session.reportInvalidToken sess token

runClientM' :: ClientM a -> SfdcClient (Either ClientError a)
runClientM' f = do
  client_env <- clientEnv
  SfdcClient $ lift $ runClientM f client_env

withAccessToken :: (AccessToken -> ClientM a) -> SfdcClient a
withAccessToken f = do
  token <- accessToken
  sess <- session
  res_e <- runClientM' $ f token
  case res_e of
    Left e -> case e of
      FailureResponse _ resp -> do
        if responseStatusCode resp == status401
          then do
            reportInvalidToken token
            token' <- accessToken
            (runClientM' $ f token') >>= fromEither
          else throwM e
      _ -> throwM e
    Right a -> return a

fromEither :: (MonadThrow m, Exception e) => Either e a -> m a
fromEither (Left e) = throwM e
fromEither (Right a) = return a