module Sfdc.Api.Session
  ( Session
  , AccessToken
  , AuthConfig (..)
  , ClientEnv

  , clientEnv

  , accessToken
  , newSession
  , reportInvalidToken
  )
where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (SomeException)
import Control.Monad.Catch (throwM)
import qualified Control.Monad.Catch as Catch
import Control.Monad.Except
import Data.Time.Clock
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS
import Numeric.Natural
import Servant.Client

import Sfdc.Api.OAuth2 (AccessToken, AuthorizationServer, PrivateKey
  , TokenResponse, instanceUrl, getAccessToken)
import qualified Sfdc.Api.OAuth2 as OAuth2

-- We have to be careful with the access token MVar. We don't want to keep
-- giving out an invalid access token once we know it's invalid. We also don't
-- want to make many requests to SFDC to get a new access token if multiple
-- threads all report an invalid token. Therefore, we take the following
-- approach:
--
-- * Keep an MVar that contains either an access token or an exception that was
--   raised when last trying to get a new access token.
-- * When a thread reports an invalid access token, we empty the MVar (so that
--   no other threads can get the invalid access token), spawn a new thread to
--   get a new access token (so that we don't, e.g., wrongly record an async
--   exception sent to the thread reporting the invalid token as being the
--   exception raised when trying to get a new token), and then put the result
--   into the MVar.
--
-- When a request is made for an access token, we raise an exception if we were
-- not able to get a token on the last refresh.
data Session = Session
  { accessTokenMVar :: MVar (Either SomeException AccessToken)
  -- This MVar should never be empty.
  , authConfig :: AuthConfig
  , httpManager :: Manager
  , clientEnv :: ClientEnv
  }

data AuthConfig = AuthConfig
  { privateKey :: PrivateKey
  , clientId :: String
  , authorizationServer :: AuthorizationServer
  , username :: String
  , tokenLifetime :: Natural
  -- ^ How long between access token request and expiration (in seconds)
  }

newSession :: AuthConfig -> IO Session
newSession auth_config = do
  token_resp <- getToken auth_config
  access_token_mvar <- newMVar (return $ OAuth2.accessToken token_resp)
  manager <- newTlsManager
  url <- parseBaseUrl . instanceUrl $ token_resp
  let client_env = mkClientEnv manager url
  return $ Session
    { accessTokenMVar = access_token_mvar
    , authConfig = auth_config
    , httpManager = manager
    , clientEnv = client_env
    }


getToken :: AuthConfig -> IO TokenResponse
getToken auth_config = do
  now <- getCurrentTime
  getAccessToken
    (privateKey auth_config)
    (clientId auth_config)
    (authorizationServer auth_config)
    (username auth_config)
    (addUTCTime (fromInteger . toInteger . tokenLifetime $ auth_config) now)

data Exception
  = CouldNotGetAccessToken SomeException
  deriving (Show)

instance Catch.Exception Exception

accessToken :: Session -> IO AccessToken
accessToken session = do
  token_e <- readMVar $ accessTokenMVar session
  case token_e of
    Left e -> throwM $ CouldNotGetAccessToken e
    Right token -> return token

refreshToken :: Session -> IO ()
refreshToken session = do
  token_resp <- (async $ getToken (authConfig session)) >>= waitCatch
  _ <- swapMVar (accessTokenMVar session) (fmap OAuth2.accessToken token_resp)
  return ()

-- Block until there's a different token in the MVar,
reportInvalidToken :: Session -> AccessToken -> IO ()
reportInvalidToken session token = do
  modifyMVar_ (accessTokenMVar session) $ \current_value ->
    case current_value of
      Right current_token ->
        if current_token == token
          then getNewToken
          else return current_value
      Left _ -> getNewToken
  where
    getNewToken :: IO (Either SomeException AccessToken)
    getNewToken = do
      res_e <- (async $ getToken (authConfig session)) >>= waitCatch
      return $ fmap OAuth2.accessToken res_e
