module Config
  ( Config (..)
  , getAuthConfig
  , getConfig
  ) where

import Crypto.PubKey.RSA as RSA (PrivateKey)
import Data.X509 (PrivKey (..))
import Data.X509.File (readKeyFile)
import Data.Yaml (decodeFileThrow)
import System.Directory

import qualified ConfigFile as CF
import Sfdc.Api.OAuth2 (AuthorizationServer)
import Sfdc.Api.Session (AuthConfig)
import qualified Sfdc.Api.Session as Sess


data Config = Config 
  { clientId :: String
  , username :: String
  , authorizationServer :: AuthorizationServer
  , privateKey :: PrivateKey
  }
  deriving (Show)

getConfig :: IO Config
getConfig = do
  let config_f_path = "test/config/config.yaml"
  config_file_exists <- doesFileExist config_f_path
  config <-
    if config_file_exists
    then decodeFileThrow config_f_path
    else error $ "Test config file not found at " ++ config_f_path

  let priv_key_path = "test/config/private_key.pem"
  priv_key_exists <- doesFileExist priv_key_path
  priv_key <-
    if priv_key_exists
    then do
      priv_key <- readKeyFile priv_key_path
      case priv_key of
        [] -> error "No private keys found"
        _:_:_ -> error "More than one private key found"
        [priv_key] ->
          case priv_key of
            PrivKeyRSA priv_key_rsa -> return priv_key_rsa
            _ -> error "Expected RSA Private Key"
    else error $ "Private key file not found at " ++ priv_key_path

  return $ Config
    { clientId = CF.clientId config
    , username = CF.username config
    , authorizationServer = CF.authorizationServer config
    , privateKey = priv_key
    }

getAuthConfig :: IO AuthConfig
getAuthConfig = do
  cfg <- getConfig
  return $ Sess.AuthConfig
    { Sess.privateKey = privateKey cfg
    , Sess.clientId = clientId cfg
    , Sess.authorizationServer = authorizationServer cfg
    , Sess.username = username cfg
    , Sess.tokenLifetime = 600
    }
