{-# LANGUAGE DeriveGeneric #-}

module ConfigFile
  (ConfigFile (..))
where

import Data.Yaml
import GHC.Generics

import Sfdc.Api.OAuth2 (AuthorizationServer)

data ConfigFile = ConfigFile
  { clientId :: String
  , username :: String
  , authorizationServer :: AuthorizationServer
  }
  deriving (Show, Generic)

instance FromJSON ConfigFile

instance FromJSON AuthorizationServer