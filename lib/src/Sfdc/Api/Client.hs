module Sfdc.Api.Client
  ( withAccessToken
  , versionedApi
  , bulkApi
  )
where

import Data.Text.Short as ST (toByteString)
import Servant.Auth
import Servant.Auth.Client
import Servant.API
import Servant.Client

import Sfdc.Api.Api
import Sfdc.Api.OAuth2


withAccessToken (AccessToken access_token) =
  let token = Token . ST.toByteString $ access_token
  in client api token

versionedApi (f :<|> _) = f "v47.0"

bulkApi (x :<|> _) = x