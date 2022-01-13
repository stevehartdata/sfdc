{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Sfdc.Api.OAuth2.HighLevel
  ( AccessToken (..)
  , AuthorizationServer (..)
  , Exception
  , PrivateKey
  , TokenResponse
  , getAccessToken
  )
where

import Servant.Client

import Control.Lens.Operators
import Control.Monad.Catch (MonadThrow, throwM)
import qualified Control.Monad.Catch as Catch
import Control.Monad.Except
import Crypto.JOSE.Error (AsError)
import Crypto.JOSE.JWK as JWK
import Crypto.JWT
import Crypto.PubKey.RSA as RSA (PrivateKey)
import Crypto.Random.Types (MonadRandom)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.String (fromString)
import Data.Text.Encoding as E (decodeUtf8)
import Data.Time.Clock
import GHC.Generics
import Network.HTTP.Client.TLS

import Sfdc.Api.OAuth2.LowLevel

data AuthorizationServer
  -- | <https://login.salesforce.com>
  = MainAuthorizationServer
  -- | <https://test.salesforce.com>
  | TestAuthorizationServer
  -- | <https://community.force.com/customers>
  | CommunityAuthorizationServer
  deriving (Eq, Show, Generic)

authorizationServerUrl MainAuthorizationServer = "login.salesforce.com"
authorizationServerUrl TestAuthorizationServer = "test.salesforce.com"
authorizationServerUrl CommunityAuthorizationServer = "community.force.com/customers"

data Exception
  = JWTException JWTError
  | ServantException ClientError
  deriving (Show)

instance Catch.Exception Exception

getAccessToken :: (MonadIO m, MonadRandom m, MonadThrow m)
               => PrivateKey
               -> String -- ^ OAuth @client_id@ or the connected app associated with the certificate corresponding to the private key
               -> AuthorizationServer
               -> String -- ^ Username of the Salesforce user or the Salesforce community user if implementing for a community
               -> UTCTime -- ^ Expiration time for the access token
               -> m TokenResponse
getAccessToken priv_key client_id auth_server username exp_time = do
  let claims = mkClaims client_id auth_server username exp_time
  jwt <- doJwtSign (JWK.fromRSA priv_key) $ claims
  let client =
        token
          (TokenRequest
            "urn:ietf:params:oauth:grant-type:jwt-bearer"
            -- TODO: encodeCompact should really return a Base64-encoded 'Text' or 'ShortText'
            (E.decodeUtf8 . BSL.toStrict . encodeCompact $ jwt))
  manager' <- newTlsManager
  let client_env =
        mkClientEnv manager' (BaseUrl Https (authorizationServerUrl auth_server) 443 "")
  res_e <- liftIO $ runClientM client client_env
  case res_e of
    Left e -> throwM $ ServantException e
    Right res -> return res

mkClaims :: String -- ^ OAuth @client_id@
         -> AuthorizationServer
         -> String -- ^ Username
         -> UTCTime -- ^ Expiration time
         -> ClaimsSet
mkClaims client_id auth_server username exp_time =
  let audience_url = authorizationServerUrl auth_server
  in
  emptyClaimsSet
    & claimIss ?~ fromString client_id
    & claimAud ?~ Audience [audience_url]
    & claimSub ?~ fromString username
    & claimExp ?~ NumericDate exp_time

doJwtSign :: (MonadRandom m, MonadThrow m)
          => JWK -> ClaimsSet -> m SignedJWT
doJwtSign jwk claims = do
  res_e <- runExceptT $ signClaims jwk (newJWSHeader ((), RS256)) claims
  case res_e of
    Left e -> throwM $ JWTException e
    Right res -> return res
